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

(defmacro ggui-edit (&rest body)
  "Set `inhibit-read-only' to t, and evaluate BODY.
Also wrap `save-excursion' for convenience."
  `(let ((inhibit-read-only t))
     (save-excursion ,@body)))

(defmacro ggui-edit-nosave (&rest body)
  "Set `inhibit-read-only' to t, and evaluate BODY.
But don't wrap `save-excursion.'"
  `(let ((inhibit-read-only t))
     ,@body))

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
(define-error 'ggui-delimiter-misplace "A ggui delimiter should/should not be here." '(error))

;;;; Etc

(defun ggui-goto (line column &optional buffer file)
  "Go to POS in BUFFER or FILE.
One of BUFFER or FILE has to be provided. If both presented, BUFFER is used.

* Arcs
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

;;;; ggui-view

;;;;; Class & init & helper

(ggui-defclass ggui-view ()
  ((overlay
    :documentation "Private. The overlay covering the range.
Don't use `slot-value' on this, instead, use the accessor `ggui--overlay'.")
   (text
    :documentation "Public. The text of the view."))
  ;; It is covered by an overlay (slot), and must have line feed and after it (which is included).
  "A range in a buffer.
Public slot: text
Virtual slot:
- beg           R&W
- end           R&W
- buffer        R
- property-list R&W")

(defun ggui-view-new (text &rest property-list)
  "Return a `ggui-view' object with TEXT.

PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (ggui-view :text text
             :overlay (apply #'ggui--make-overlay
                             1 1
                             (get-buffer-create " *ggui-tmp*")
                             property-list)))

(cl-defmethod ggui--presentp ((view ggui-view))
  "Return t if VIEW is inserted into some buffer, nil if not."
  ;; if the overlay is in tmp buffer, it is no present.
  (not (eq (overlay-buffer (ggui--overlay view)) (get-buffer-create " *ggui-tmp*"))))

(defun ggui--translate-pos (line column buffer)
  "Translate LINE:COLUMN in BUFFER to POS in BUFFER.
Return (POS BUFFER)."
  (cons (progn (ggui-goto line column buffer) (point))
        buffer))

(defun ggui--make-overlay (beg end &optional buffer &rest property-list)
  "Return an overlay from BEG to END in BUFFER.

See `ggui-decode-pos' for options on BEG and END.
Use current buffer if BUFFER is nil. PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (let ((overlay (make-overlay beg end buffer nil t)))
    ;; marker: insert in front: included, end: included
    (while property-list
      (overlay-put overlay (pop property-list) (pop property-list)))
    overlay))

;;;;; Beg & end & buffer

;; enable if they are actually needed

;; (cl-defmethod ggui--beg-mark ((view ggui-view))
;;   "Return the beginning of VIEW as a marker."
;;   (let ((overlay (ggui--overlay view)))
;;     (set-marker (make-marker) (overlay-start overlay) (overlay-buffer overlay))))

;; (cl-defmethod ggui--end-mark ((view ggui-view))
;;   "Return the end of VIEW as a marker."
;;   (let ((overlay (ggui--overlay view)))
;;     (set-marker (make-marker) (overlay-end overlay) (overlay-buffer overlay))))

;; (cl-defmethod ggui--beg ((view ggui-view))
;;   "Return the beginning of VIEW as a point."
;;   (overlay-start (ggui--overlay view)))

;; (cl-defmethod (setf ggui--beg) (beg (view ggui-view))
;;   "Set BEG of VIEW."
;;   (ggui-move-overlay view beg))

;; (cl-defmethod ggui--end ((view ggui-view))
;;   "Return the end of VIEW as a point."
;;   (overlay-end (ggui--overlay view)))

;; (cl-defmethod (setf ggui--end) (end (view ggui-view))
;;   "Set END of VIEW."
;;   (ggui-move-overlay view end))

;; (cl-defmethod ggui--buffer ((view ggui-view))
;;   "Return the buffer of VIEW."
;;   (overlay-buffer (ggui--overlay view)))

(cl-defmethod ggui--move-overlay ((view ggui-view) beg end &optional buffer)
  "Move overlay of VIEW to BEG END in BUFFER.
If BEG, END or BUFFER omitted, don't change it.
Return nil."
  ;; basically create a new one, set values, delete the old one
  (let ((old-ov (ggui--overlay view))
        new-ov)
    (setq new-ov (apply #'ggui--make-overlay
                        (or beg (overlay-start old-ov))
                        (or end (overlay-end old-ov))
                        (or buffer (overlay-buffer old-ov))
                        (overlay-properties old-ov)))
    (setf (ggui--overlay view) new-ov)
    (delete-overlay old-ov)) ; Don't forget to cleanup -- Izayoi Sakuya
  nil)

;;;;; Delimiter

(defun ggui--delimiter ()
  "Return a delimiter."
  (propertize "\n" 'invisible t 'ggui-delimeter t))

(defun ggui--2delimiter ()
  "Return two delimiter together."
  (propertize "\n\n" 'invisible 'ggui-delimeter t))

(defun ggui--insert-delimiter ()
  "Insert a delimiter at point. Return nil.
When return, point is at the end of the delimiters."
  (ggui-edit-nosave (insert (ggui--delimeter))))

(defun ggui--insert-2delimiter ()
  "Insert two delimiters at point. Return nil.
When return, point is at the end of delimiters."
  (ggui-edit-nosave (insert (ggui--2delimiter))))

;;;;; Put/insert before/after

(defun ggui--check-delimiter (&optional no-left no-right)
  "Check if delimiters are correctly setup.
Should: one and only one before point, one and only one after point."
  (let ((point (point)))
    (cond ((not (member 'ggui-delimiter (text-properties-at (1- point))))
           (signal 'ggui-delimiter-misplace (list "Left delimiter is missing, point at" point "delimiter should be at" (1- point))))
          ((member 'ggui-delimiter (text-properties-at (- point 2)))
           (signal 'ggui-delimiter-misplace (list "Extra delimiter left to left delimiter, point at" point "extra delimiter point at" (- point 2))))
          ((not (member 'ggui-delimiter (text-properties-at point)))
           (signal 'ggui-delimiter-misplace (list "Right delimiter is missing, point at" point "delimiter should be at" point)))
          ((member 'ggui-delimiter (text-properties-at (1+ point)))
           (signal 'ggui-delimiter-misplace (list "Extra delimiter right to right delimiter, point at" point "extra delimiter point at" (1+ point)))))))

;;;;;; String

(cl-defmethod ggui-put-before ((view ggui-view) (str string))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui-edit
   (goto-char (1- (ggui--beg view)))
   ;; now we are between the two delimiters
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (backward-char)
   (insert str)))

(cl-defmethod ggui-put-after ((view ggui-view) (str string))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui-edit
   (goto-char (1+ (ggui--end view)))
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (forward-char)
   (insert str)))

(cl-defmethod ggui-insert-before ((view ggui-view) (str string))
  "Insert STR in front of VIEW. VIEW cover STR.
Return nil."
  (ggui-edit
   (goto-char (ggui--beg view))
   (insert str)))

(cl-defmethod ggui-insert-after ((view ggui-view) (str string))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui-edit
   (goto-char (ggui--end view))
   (insert str)))

;;;;;; View

(cl-defmethod ggui-put-before ((view ggui-view) (aview ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui-edit
   (goto-char (1- (ggui--beg view)))
   ;; now we are between the two delimiters
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (backward-char)
   (ggui--move-overlay aview (point) (point))
   (insert (ggui-text aview))))

(cl-defmethod ggui-put-after ((view ggui-view) (aview ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui-edit
   (goto-char (1+ (ggui--end view)))
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (forward-char)
   (ggui--move-overlay aview (point) (point))
   (insert (ggui-text aview))))

(provide 'ggui)

;;; ggui ends here
