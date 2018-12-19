;;; ggui.el --- Grand GGUD User Interface      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuan Fu.

;; Author: Yuan Fu <casouri@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; Keywords: extensions
;; Package-Requires: ((emacs ""))
;; Version: 0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

;;; Code:

(require 'eieio)

;;;; Helper

(defmacro ggui-defclass (classname superclass-list slot-list &rest rest)
  "A thin wrapper around `defclass'.
Expands to (defclass CLASSNAME SUPERCLASS-LIST SLOT-LIST REST).

Additionally, it gives you a free initarg, writer and reader
in following conventions: (slotname is the slot name)
:initarg :slotname
:writer gguir-slotname
:reader gguiw-slotname

gguir-xxx stands for ggui--read-xxx, gguiw-xxx stands for ggui--write-xxx.

Any user defined slot options will override these automatically generated ones."
  (declare (indent 2))
  `(defclass ,classname ,superclass-list
     ,(mapcar (lambda (slot-form)
                (let ((slot-name (symbol-name (car slot-form))))
                  (append slot-form
                          ;; slot options appeared earlier
                          ;; takes precedents
                          (list :initarg
                                (intern (format ":%s" slot-name))
                                :accessor
                                (intern (format "ggui--%s" slot-name))
                                ;; :reader
                                ;; (intern (format "gguir-%s" slot-name))
                                ;; :writer
                                ;; (intern (format "gguiw-%s" slot-name))
                                ))))
              slot-list)
     ,@rest))

(defmacro ggui-signal (signal-symbol str &rest args)
  "Signal SIGNAL-SYMBOL with helpful information.
Basically a `signal' but the first data is a formatted string.

STR is format string, ARGS are like that in `format'."
  `(signal ,signal-symbol (list (format-message ,str ,@args))))

;; FIXME I set it to :info for develop reason
(defvar ggui-log-level :info
  "Log level of ggui, can be :error, :warning or :info.")

(defvar ggui--log-level-plist '(:error 3 :warning 2 :info 1)
  "Plist used to know which level is more bad ass.")

(defun ggui-log (level str &rest args)
  "Same as `message'.
STR and ARGS are like those in format.
LEVEL is a symbol, can be :info :warn or :error."
  (when (>= (plist-get ggui--log-level-plist level) ; this log is more bad ass than setting
            (plist-get ggui--log-level-plist ggui-log-level))
    (apply #'message
           (format "GGUI %s %s: %s"
                   (format-time-string "%Y-%m-%d %T" (current-time))
                   (plist-get '(:warn "Warning" :error "Error" :info "Info") level)
                   str)
           args)))

;;;; Error

;; TODO what error condition does this error belong to?
(define-error 'ggui-pos-out-of-range "Position is out of the range of the buffer (pos showed after colon)" '(error))
(define-error 'ggui-buffer-missing "There is no such buffer" '(file-missing error))
(define-error 'ggui-end-of-line "The line is not long enough" '(end-of-buffer error))

;;;; Etc

(defun ggui-goto (line column &optional buffer file)
  "Go to POS in BUFFER or FILE.
One of BUFFER or FILE has to be provided. If both presented, BUFFER is used.

* Args
- LINE :: 0-based, nil means 0
- COLUMN :: 0-based, nil means 0
- BUFFER :: string or a buffer
- FILE :: absolute path to file

* Return
nil

* Error
- `pos-out-of-range' :: if POS is out of the range of the buffer it's in.
- `file-missing'
- `buffer-missing'"
  ;; switch to buffer/file
  (cond (buffer  (switch-to-buffer (or (get-buffer buffer) (signal 'ggui-buffer-missing buffer))))
        (file (if (file-exists-p file) (find-file file) (signal 'file-missing file))) ;; TODO find-file-literally?
        (t (signal 'wrong-number-of-arguments (list "You need to supply either BUFFER or FILE, however, both are nil."))))
  ;; go to position
  (goto-char 1)
  (condition-case nil
      (progn (forward-line (or line 0))
             ;; dotimes is inclusive
             (dotimes (_ (or column 0))
               (if (eq (char-after) ?\n)
                   (signal 'ggui-end-of-line (list "LINE" line "in BUFFER" buffer "doesn't have COLUMN" column))
                 (forward-char)))
             nil)
    (end-of-buffer (signal 'ggui-pos-out-of-range (list "LINE:" line "COLUMN:" column)))))

;;;; ggui-range

(ggui-defclass ggui-range ()
  ((overlay
    :documentation "Private. The overlay covering the range.
Don't use `slot-value' on this, instead, use the accessor `ggui--overlay'."))
  ;; It is covered by an overlay (slot), and must have line feed and after it (which is included).
  "A range in a buffer.")

(defun ggui-range-new (beg end &optional buffer property-list)
  "Return a `ggui-range' object from BEG to END in BUFFER.

See `ggui-decode-pos' for options on BEG and END.

PROPERTY-LIST is a list of overlay property & value pairs.
It looks like ((PROPERTY VALUE) (PROPERTY VALUE)...)."
  (ggui-range :overlay (ggui--make-overlay beg end buffer property-list)))

(defun ggui-translate-pos (line column buffer)
  "Translate LINE:COLUMN in BUFFER to POS in BUFFER.
Return (POS BUFFER)."
  )

(defun ggui-decode-pos (beg end &optional buffer)
  "Translate BEG END and BUFFER.

BEG and END can be:
1. point
2. a list like (LINE COLUMN), if LINE or COLUMN is nil, it will error.
3. marker
BEG and END don't have to be in the same category,
but have to be in the same buffer (if specified).

If BEG or END is marker, BUFFER is ignored.
Use current buffer if BUFFER is nil.

Return a list of (BEG END BUFFER)"
  (let* ((beg&end ; process BEG and END and turn then into point
          (mapcar
           (lambda (pos) ; returns (pos . buffer)
             (pcase pos
               ((pred integerp) (when (<= pos 0) (signal 'invalid-argument (list "pos <= 0:" pos)))
                (pos . nil))
               ((pred listp) (cons (or (progn (ggui-goto (car pos) (cadr pos) buffer) (point))
                                       (signal 'invalid-argument
                                               (list "Invalid position list, should be (LINE COLUMN), got" pos)))
                                   nil))
               ((pred markerp) (cons (marker-position pos) (marker-buffer pos)))
               (_ (signal 'invalid-argument
                          (list "Invalid position, not point,list or marker, what IS this anyway?:" pos)))))
           (list beg end))) ; with mapcar you get ((pos-beg . buffer) (pos-end . buffer)) where buffers could be nil
         (beg0 (caar beg&end))
         (end0 (caadr beg&end))
         (buf-beg (cdar beg&end))
         (buf-end (cdadr beg&end)))
    ;; when both are valid buffer but point to different buffers.
    ;; not use `buffer-live-p' to make is strict.
    (when (and (not buf-beg) (not buf-end) (not (equal buf-beg buf-end)))
      (signal 'invalid-argument
              (list "BEG and END are from different buffers, BEG:" buf-beg "END:" buf-end)))
    (list beg0 end0 (or buf-beg buf-end buffer (current-buffer)))))

(defun ggui--make-overlay (beg end &optional buffer property-list)
  "Return an overlay from BEG to END in BUFFER.

See `ggui-decode-pos' for options on BEG and END.

Use current buffer if BUFFER is nil.
For PROPERTY-LIST see `ggui-range-new'."
  (save-excursion
    (let ((pos-list (ggui-decode-pos beg end buffer))
          overlay)
      ;; marker: insert in front: included, end: not included
      (setq overlay (apply #'make-overlay pos-list))
      (mapc (lambda (lst) (overlay-put overlay (car lst) (cadr lst)))
            property-list)
      overlay)))

(cl-defmethod ggui--beg-mark ((range ggui-range))
  "Return the beginning of RANGE as a marker."
  (let ((overlay (ggui--overlay range)))
    (set-marker (make-marker) (overlay-start overlay) (overlay-buffer overlay))))

(cl-defmethod ggui--end-mark ((range ggui-range))
  "Return the end of RANGE as a marker."
  (let ((overlay (ggui--overlay range)))
    (set-marker (make-marker) (overlay-end overlay) (overlay-buffer overlay))))

(cl-defmethod ggui--beg ((range ggui-range))
  "Return the beginning of RANGE as a point."
  (overlay-start (ggui--overlay range)))

(cl-defmethod (setf ggui--beg) (beg (range ggui-range))
  "Set BEG of RANGE."
  (ggui-update-pos range beg))

(cl-defmethod ggui--end ((range ggui-range))
  "Return the end of RANGE as a point."
  (overlay-end (ggui--overlay range)))

(cl-defmethod (setf ggui--end) (end (range ggui-range))
  "Set END of RANGE."
  (ggui-update-pos range end))

(cl-defmethod ggui--buffer ((range ggui-range))
  "Return the buffer of RANGE."
  (overlay-buffer (ggui--overlay range)))

(cl-defmethod ggui-update-pos ((range ggui-range) beg end &optional buffer)
  "Update the range of RANGE to BEG to END in BUFFER.
If BEG, END or BUFFER omitted, don't change it.

Return nil."
  (let ((old-ov (ggui--overlay range))
        new-ov)
    (setq new-ov (ggui--make-overlay (or beg (overlay-start old-ov))
                                     (or end (overlay-end old-ov))
                                     (or buffer (overlay-buffer old-ov))))
    (setf (ggui--overlay range) new-ov)
    (delete-overlay old-ov)) ; Don't forget to cleanup -- Izayoi Sakuya
  nil)

(cl-defmethod ggui-put-before ((range ggui-range) (str string))
  "Insert STR in front of RANGE. RANGE doesn't cover STR.
Return nil."
  (save-excursion
    (goto-char (ggui--beg range))
    (insert str)
    (ggui-update-pos range (point)))
  nil)

(cl-defmethod ggui-put-after ((range ggui-range) (str string))
  "Insert STR in front of RANGE. RANGE doesn't cover STR.
Return nil."
  (save-excursion
    (goto-char (ggui--end range))
    (insert str))
  nil)

(cl-defmethod ggui-insert-before ((range ggui-range) (str string))
  "Insert STR in front of RANGE. RANGE cover STR.
Return nil."
  (save-excursion
    (goto-char (ggui--beg range))
    (insert str))
  nil)

(cl-defmethod ggui-insert-after ((range ggui-range) (str string))
  "Insert STR in front of RANGE. RANGE doesn't cover STR.
Return nil."
  (save-excursion
    (goto-char (ggui--beg range))
    (insert str))
  nil)

(provide 'ggui)

;;; ggui ends here
