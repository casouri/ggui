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
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

;;;; Helper

(defmacro ggui--edit (&rest body)
  "Set `inhibit-read-only' to t, and evaluate BODY.
Also wrap `save-excursion' for convenience."
  `(let ((inhibit-read-only t))
     (save-excursion ,@body)))

(defmacro ggui--edit-nosave (&rest body)
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

(defvar ggui--log-level-plist '(:error 3 :warn 2 :info 1)
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
                   (plist-get '(:warn "Warn" :error "Error" :info "Info") level)
                   str)
           args)))

(defmacro ggui--push-end (elt lst)
  "Push ELT to the end of LST."
  ;; (setq ll (list 1 2 3))
  ;; (benchmark-run 1000 (ggui--push-end 0 ll))
  ;; (0.099558 1 0.0901969999999892)
  `(setf ,lst (append ,lst (list ,elt))))

(defun ggui--fix-len-visual (str len)
  "Adjust STR to have length LEN, either by chopping or padding.

LEN must be a visual length, i.e. returned by `ggui--visual-length'.

Visual means to things:
1. length is measured in visual way, i.e., CJK chars occupy two units.
2. \"..\" is added to the end of STR when chopping
unless LEN is less than 2.

If STR is shorter than LEN, spaces are padded on the SIDE."
  (let ((real-len (ggui--visual-length str)))
    (if (> real-len len)
        (if (> len 2)
            (concat (ggui--visual-substring str 0 (- len 2)) "..")
          (ggui--visual-substring str 0 len))
      (concat str (make-string (- len real-len) ?\s)))))

;;;;; CJK length

(defun ggui--hanp (char)
  "Return t if CHAR is a han character.

See CJK Unified Ideographs (Han) in http://www.unicode.org/charts/.

This function simply checks if 4E00 < char < 9FFF."
  (< 19968 char 40959))

(defun ggui--cjkp (char)
  "Return t if CHAR is CJK character."
  ;;TODO japanese
  (ggui--hanp char))

(defun ggui--visual-length (str)
  "Return visual length of STR.

E.g.,CJK characters are counted as 2 units in length.
This function doesn't respect 'display or 'invisible text property."
  (string-width str))

(defun ggui--visual-substring (str from to)
  "Return the substring of STR from FROM to TO.

Similar to `ggui--visual-length', CJK chars are counted as two unit.
If TO or FROM is in the middle of a CJK char, throw away the char and append a space.

This function doesn't respect 'display or 'invisible text property.
TODO Truly support CJK, instead of just han."
  ;; ??? appended space inherit text properties of the char next to it?
  (if (< to from)
      (signal 'args-out-of-range (list "TO < FROM, TO:" to "FROM:" from))
    (let ((visual-from 0) ; visual index on display
          (visual-to 0)
          (real-from 0) ; actual index in string
          (real-to 0)
          before-pad
          after-pad)
      ;; from index
      (while (< visual-from from)
        (setq visual-from
              (if (ggui--cjkp (aref str real-from))
                  (+ 2 visual-from)
                (1+ visual-from)))
        (setq real-from (1+ real-from)))
      (if (> visual-from from)
          (progn (setq before-pad " ")
                 (setq visual-to (1- visual-from))
                 (setq real-to real-from))
        (setq before-pad ""))

      ;; to index
      (while (< visual-to to)
        (setq visual-to
              (if (ggui--cjkp (aref str real-to))
                  (+ 2 visual-to)
                (1+ visual-to)))
        (setq real-to (1+ real-to)))
      (if (> visual-to to)
          (progn (setq after-pad " ")
                 (setq real-to (1- real-to)))
        (setq after-pad ""))
      ;; return string
      (concat before-pad
              (substring str real-from real-to)
              after-pad))))

;;;; Error

;; TODO what error condition does this error belong to?
(define-error 'ggui-pos-out-of-range "Position is out of the range of the buffer (pos showed after colon)" '(error))
(define-error 'ggui-buffer-missing "There is no such buffer" '(file-missing error))
(define-error 'ggui-app-missing "There is no app." '(error))
(define-error 'ggui-end-of-line "The line is not long enough" '(end-of-buffer error))
(define-error 'ggui-delimiter-misplace "A ggui delimiter should/should not be here." '(error))
(define-error 'ggui-prohibit-edit "This edition is not allowed" '(error))
(define-error 'ggui-view-not-present "This view is not on any buffer." '(error))

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
    :type (satisfies overlayp)
    :documentation "Private. The overlay covering the range.
Don't use `slot-value' on this, instead, use the accessor `ggui--overlay'.")
   (text
    :type string
    :documentation "Public. The text of the view.")
   (property-list
    :documentation "Dummy slot used by init: (PROP VALUE PROP VALUE).
Use `ggui--overlay-put' to add properties to overlay."))
  ;; It is covered by an overlay (slot), and must have line feed and after it (which is included).
  "A piece of overlay-managed text.
Public slot: text
Virtual slot:
- beg           R&W
- end           R&W
- buffer        R
- property-list R&W")

(cl-defmethod object-print ((view ggui-view) &rest str)
  (apply #'cl-call-next-method
         view
         (format " OVERLAY: %S TEXT: %s" (ggui--overlay view) (ggui--text view))
         str))

(cl-defmethod initialize-instance :after ((view ggui-view) &rest _)
  "Setup overlay and property."
  (setf (ggui--overlay view)
        (apply #'ggui--make-overlay
               1 1 (get-buffer-create " *ggui-tmp*")
               (ggui--property-list view)))
  (overlay-put (ggui--overlay view) 'ggui-view view)
  (slot-makeunbound view 'property-list))

(defun ggui-view-new (text &rest property-list)
  "Return a `ggui-view' object with TEXT.

PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (ggui-view :text text :property-list property-list))

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

Use current buffer if BUFFER is nil. PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (let ((overlay (make-overlay beg end buffer nil t)))
    ;; marker: insert in front: included, end: included
    (while property-list
      (overlay-put overlay (pop property-list) (pop property-list)))
    overlay))

(cl-defmethod (setf ggui--text) :after (text (view ggui-view))
  "Update display in buffer."
  (when (ggui--presentp view)
    (with-current-buffer (ggui--buffer view)
      (ggui--edit
       (goto-char (ggui--beg view))
       (delete-region (ggui--beg view) (ggui--end view))
       (insert text)))))

;;;;; Beg & end & buffer

;; enable if they are actually needed

(cl-defmethod ggui--beg-mark ((view ggui-view))
  "Return the beginning of VIEW as a marker."
  (let ((overlay (ggui--overlay view)))
    (set-marker (make-marker) (overlay-start overlay) (overlay-buffer overlay))))

(cl-defmethod ggui--end-mark ((view ggui-view))
  "Return the end of VIEW as a marker."
  (let ((overlay (ggui--overlay view)))
    (set-marker (make-marker) (overlay-end overlay) (overlay-buffer overlay))))

(cl-defmethod ggui--beg ((view ggui-view))
  "Return the beginning of VIEW as a point."
  (overlay-start (ggui--overlay view)))

(cl-defmethod ggui--end ((view ggui-view))
  "Return the end of VIEW as a point."
  (overlay-end (ggui--overlay view)))

(cl-defmethod ggui--buffer ((view ggui-view))
  "Return the buffer of VIEW."
  (overlay-buffer (ggui--overlay view)))

(cl-defmethod ggui--overlay-put ((view ggui-view) &rest property-list)
  "Put properties to VIEW's overlay.
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result.
Return nil."
  (let ((overlay (ggui--overlay view)))
    (while property-list
      (overlay-put overlay (pop property-list) (pop property-list)))
    nil))

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
                        buffer
                        (overlay-properties old-ov)))
    (setf (ggui--overlay view) new-ov)
    (delete-overlay old-ov)) ; Don't forget to cleanup -- Izayoi Sakuya
  nil)

;;;;; Delimiter

(defun ggui--delimiter ()
  "Return a delimiter."
  (propertize "\n" 'invisible t 'ggui-delimiter t))

(defun ggui--2delimiter ()
  "Return two delimiter together."
  (propertize "\n\n" 'invisible t 'ggui-delimiter t))

(defun ggui--insert-delimiter ()
  "Insert a delimiter at point. Return nil.
When return, point is at the end of the delimiters."
  (ggui--edit-nosave (insert (ggui--delimiter))))

(defun ggui--insert-2delimiter ()
  "Insert two delimiters at point. Return nil.
When return, point is at the end of delimiters."
  (ggui--edit-nosave (insert (ggui--2delimiter))))

;;;;; Put/insert before/after

;;;;;; Generic
;;
;; Implement at least ggui-put-before/after for
;; generic views you want to support.

(cl-defgeneric ggui-put-before (str view)
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil.")

(cl-defgeneric ggui-put-after (str view)
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil.")

(defun ggui--check-delimiter ()
  "Check if delimiters are correctly setup.
Should: one and only one before point, one and only one after point."
  (let ((point (point)))
    (cond ((not (plist-get (text-properties-at (1- point)) 'ggui-delimiter))
           (signal 'ggui-delimiter-misplace (list "Left delimiter is missing, point at" point "delimiter should be at" (1- point))))
          ((plist-get (text-properties-at (- point 2)) 'ggui-delimiter)
           (signal 'ggui-delimiter-misplace (list "Extra delimiter left to left delimiter, point at" point "extra delimiter point at" (- point 2))))
          ((not (plist-get (text-properties-at point) 'ggui-delimiter))
           (signal 'ggui-delimiter-misplace (list "Right delimiter is missing, point at" point "delimiter should be at" point)))
          ((plist-get (text-properties-at (1+ point)) 'ggui-delimiter)
           (signal 'ggui-delimiter-misplace (list "Extra delimiter right to right delimiter, point at" point "extra delimiter point at" (1+ point)))))))

;;;;;; Check

(cl-defmethod ggui-put-before :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui--top-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be put before a `ggui--top-view', you try to put:" stuff)))
  (unless (ggui--presentp view)
    (signal 'ggui-view-not-present (list "VIEW is not present yet, you can't put STUFF before it."))))

(cl-defmethod ggui-put-after :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui--bottom-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be put after a `ggui--bottom-view', you try to put:" stuff)))
  (unless (ggui--presentp view)
    (signal 'ggui-view-not-present (list "VIEW is not present yet, you can't put STUFF before it."))))

(cl-defmethod ggui-insert-before :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui--top-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be inserted before a `ggui--top-view', you try to insert:" stuff)))
  (when (eq ggui--bottom-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be inserted before a `ggui--bottom-view', you try to insert:" stuff))))

(cl-defmethod ggui-insert-after :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui--top-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be inserted after a `ggui--top-view', you try to insert:" stuff)))
  (when (eq ggui--bottom-view view)
    (signal 'ggui-prohibit-edit (list "Nothing can be inserted after a `ggui--bottom-view', you try to insert:" stuff))))

;;;;;; String (probably shouldn't add string to views)

(cl-defmethod ggui-put-before ((str string) (view ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui--edit
   (goto-char (1- (ggui--beg view)))
   ;; now we are between the two delimiters
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (backward-char)
   (insert str)))

(cl-defmethod ggui-put-after ((str string) (view ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui--edit
   (goto-char (1+ (ggui--end view)))
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (forward-char)
   (insert str)))

(cl-defmethod ggui-insert-before ((str string) (view ggui-view))
  "Insert STR in front of VIEW. VIEW cover STR.
Return nil."
  (ggui--edit
   (goto-char (ggui--beg view))
   (insert str)))

(cl-defmethod ggui-insert-after ((str string) (view ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui--edit
   (goto-char (ggui--end view))
   (insert str)))

;;;;;; View

(cl-defmethod ggui-put-before ((aview ggui-view) (view ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui--edit
   (goto-char (1- (ggui--beg view)))
   ;; now we are between the two delimiters
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (backward-char)
   ;; TODO: investigate into invisible overlay:
   ;; does front/rear advance work as normal?
   (ggui--move-overlay aview (point) (point))
   (insert (ggui--text aview))))

(cl-defmethod ggui-put-after ((aview ggui-view) (view ggui-view))
  "Insert STR in front of VIEW. VIEW doesn't cover STR.
Return nil."
  (ggui--edit
   (goto-char (1+ (ggui--end view)))
   (ggui--check-delimiter)
   (ggui--insert-2delimiter)
   (backward-char)
   (ggui--move-overlay aview (point) (point))
   (insert (ggui--text aview))))

;;;;;;; Remove before insert

(cl-defmethod ggui-put-before :before ((aview ggui-view) (_ ggui-view))
  "Remove AVIEW's display before adding it."
  (when (ggui--presentp aview) (ggui--remove-display aview)))

(cl-defmethod ggui-put-after :before ((aview ggui-view) (_ ggui-view))
  "Remove AVIEW's display before adding it."
  (when (ggui--presentp aview) (ggui--remove-display aview)))


;;;;; Remove
(cl-defmethod ggui--remove-display ((view ggui-view))
  "Remove VIEW's presence from buffer."
  (when (ggui--presentp view)
    (ggui--edit
     (let (beg end)
       ;; check left
       (goto-char (ggui--beg-mark view))
       (backward-char)
       (ggui--check-delimiter)
       (setq beg (point))
       ;; check right
       (goto-char (ggui--end view))
       (forward-char)
       (ggui--check-delimiter)
       (setq end (point))
       ;; move overlay and remove text
       (ggui--move-overlay view 1 1 (get-buffer-create " *ggui-tmp*"))
       (delete-region beg end)))))

;;;; ggui managed buffer
;;
;; One such buffer has a top and a bottom anchor
;; to insert `ggui-view's and is read only.

(defvar-local ggui--setup nil
  "Whether current buffer is setup.")
(defvar-local ggui--top-view nil
  "The top most (invisible) view of buffer.")
(defvar-local ggui--bottom-view nil
  "The bottom most (invisible) view of buffer.")
(defconst ggui--top-text "T" "(Invisible) text of `ggui--top-view'.")
(defconst ggui--bottom-text "B" "(Invisible) text of `ggui--bottom-view'.")
(defconst ggui-point-min 3
  "Point 1 is `ggui--top-text', point 2 is a delimiter, point 3 is another delimiter,
point 4 is the first (user defined) view.")

(defun ggui--setup-buffer (buffer)
  "Setup BUFFER. BUFFER can be either a string or a buffer."
  (let ((buffer (cond ((bufferp buffer) buffer)
                      ((stringp buffer) (get-buffer-create buffer))
                      (t (signal 'invalid-argument (list "BUFFER is not a buffer nor a string" buffer))))))
    (unless (buffer-live-p buffer) (signal 'ggui-buffer-missing (list "BUFFER is not a live buffer" buffer)))
    ;; setup
    (with-current-buffer buffer
      (if ggui--setup
          (ggui-log :warn "Buffer %S already set up but someone try to set up again." buffer)
        ;; we manually insert view text and setup overlay
        (ggui--edit
         (erase-buffer)
         (setq-local ggui--top-view (ggui-view-new ggui--top-text 'invisible t))
         (setq-local ggui--bottom-view (ggui-view-new ggui--bottom-text 'invisible t))
         ;; insert T\n\nB
         (goto-char 1)
         (insert (ggui--text ggui--top-view))
         (ggui--insert-2delimiter)
         (insert (ggui--text ggui--bottom-view))
         ;; put overlay
         (ggui--move-overlay ggui--top-view 1 2)
         (ggui--move-overlay ggui--bottom-view 4 5)
         (setq ggui--setup t)
         (read-only-mode)))))
  nil)

;;;;; ggui-object-at-point

(defun ggui-object-at-point ()
  "Return the ggui object at point."
  (interactive)
  (catch 'found
    (if ggui--setup
        (dolist (ov (overlays-at (point)))
          (when-let ((obj (plist-get (overlay-properties ov) 'ggui-view)))
            (throw 'found obj)))
      (message "No ggui related object at point"))))

;;;; Toggleable

(cl-defgeneric ggui-toggle-forward (_)
  "Toggle the object's state forward."
  (signal 'cl-no-applicable-method))

(cl-defgeneric ggui-toggle-backward (_)
  "Toggle the object's state backward."
  (signal 'cl-no-applicable-method))

(cl-defgeneric ggui-toggle-to (_ state-index)
  "Toggle to the STATE-INDEX th state of the toggleable."
  (signal 'cl-no-applicable-method (list "STATE-INDEX is" state-index)))

(defun ggui-toggle-at-point (n)
  "Toggle the stuff at point (if it relates to a ggui object).
If provided with numeric prefix argument, toggle forward N times.
If N is negative, toggle backward N times."
  (interactive "p")
  (when-let ((obj (ggui-object-at-point)))
    (dotimes (_ (abs n))
      (if (>= n 0)
          (ggui-toggle-forward obj)
        (ggui-toggle-back obj)))))

;;;; Hideshow

(cl-defgeneric ggui-hide (_)
  "Hide the display.")

(cl-defgeneric ggui-show (_)
  "Show the display.")

(defclass ggui-hideshow (ggui-view)
  ()
  "View can hide/show.")

(cl-defmethod ggui-hide ((view ggui-hideshow))
  (if (ggui--presentp view)
      (ggui--overlay-put view 'invisible t)
    (signal 'ggui-view-not-present)))

(cl-defmethod ggui-show ((view ggui-hideshow))
  (if (ggui--presentp view)
      (ggui--overlay-put view 'invisible nil)
    (signal 'ggui-view-not-present)))

;;;; List and tree
;;
;; Use the list manipulation functions below
;; to keep `ggui-view's in the list in sync
;; when editing lists.
;;
;; For other generic sequences,
;; implement these generic functions for sequence:
;;
;; ggui-delq
;; ggui-delq-keep-length
;; ggui-insert-at
;; seq-elt
;; seq-find
;; seq-length
;; (setf seq-elt)

;;;;; ggui generic list function

(cl-defgeneric ggui-delq (elt seq &optional compare-fn)
  "Delete ELT from SEQ.
Return the modified SEQ for convenience.

If COMPARE-fn nil, use `eq', other options are `equal' and `eql'.")

(cl-defgeneric ggui-delq-keep-length (elt seq &optional compare-fn)
  "Delete ELT from SEQ but keep length.
That is, for elements that `eq' to ELT,
set them to nil
Return the modified SEQ for convenience.

If COMPARE-fn nil, use `eq', other options are `equal' and `eql'.")

(cl-defgeneric ggui-insert-at (elt seq n)
  "Insert ELT into SEQ at Nth position. Destructive.
Return SEQ for convenience.")

(cl-defgeneric ggui-put-at (elt seq n)
  "Put ELT into SEQ at Nth position. Destructive.
If ELT is already in seq, it is removed first, note that
N regards the SEQ after removing ELT.")


;;;;; Method implementation for list

(cl-defmethod ggui-delq (elt seq &optional compare-fn)
  (let ((new-seq (remove nil (ggui-delq-keep-length elt seq compare-fn))))
    (setcar seq (car new-seq))
    (setcdr seq (cdr new-seq)))
  seq)

(cl-defmethod ggui-delq-keep-length (elt (seq list) &optional compare-fn)
  (let ((len (length seq))
        (index 0)
        (compare-fn (or compare-fn 'eq)))
    (while (< index len)
      (when (funcall compare-fn elt (nth index seq))
        (setf (nth index seq) nil))
      (cl-incf index)))
  seq)

(cl-defmethod ggui-insert-at (elt (seq list) n)
  (setf (nthcdr n seq) (cons elt (nthcdr n seq)))
  seq)

;;;;; Implementation for ggui-view

;; no side effect for ggui-insert-at because you shouldn't use it
;; directly

(cl-defmethod ggui-insert-at ((view ggui-view) (seq sequence) n)
  ;; do side effect first so we don't worry about position N
  ;; changes when inserting VIEW
  (let* ((len (seq-length seq)))
    (if (= n len)
        (ggui-put-after view (seq-elt seq (1- len)))
      (ggui-put-before view (seq-elt seq n))))
  (cl-call-next-method view seq n))

(cl-defmethod ggui-delq-keep-length :before ((view ggui-view) (seq sequence) &optional _)
  (if (seq-find (lambda (elt) (eq elt view)) seq)
      (ggui--remove-display view)))

;; doesn't do any thing since in our implementation,
;; `ggui-delq' uses `ggui-delq-keep-length'
(cl-defmethod ggui-delq :after ((_ ggui-view) _ &optional _)
  nil)

;;;;; Implementation for sequence

;; identical
(cl-defmethod ggui-insert-at ((view sequence) (seq sequence) n)
  ;; do side effect first so we don't worry about position N
  ;; changes when inserting VIEW
  (let* ((len (seq-length seq)))
    (if (= n len)
        (ggui-put-after view (seq-elt seq (1- len)))
      (ggui-put-before view (seq-elt seq n))))
  (cl-call-next-method view seq n))

(cl-defmethod ggui-delq-keep-length :before ((view sequence) (seq sequence) &optional _)
  (if (seq-find (lambda (elt) (eq elt view)) seq)
      (ggui--remove-display view)))

(cl-defmethod ggui-delq :after ((_ sequence) _ &optional _)
  nil)

;;;;; Methods for ggui-view functions for list

;; seq to view
(cl-defmethod ggui-put-before ((seq sequence) (view ggui-view))
  "Put every view in SEQ before VIEW, in normal order.
E.g., SEQ: (1 2 3 4) VIEW: 5 -> 1 2 3 4 5.
This function is recursive.
Return nil."
  (ggui-put-before (seq-elt seq 0) view)
  (ggui-put-after (seq-subseq seq 1) (seq-elt seq 0))
  nil)

(cl-defmethod ggui-put-after ((seq sequence) (view ggui-view))
  "Put every view in SEQ after VIEW, in normal order.
E.g., SEQ: (2 3 4 5) VIEW: 1 -> 1 2 3 4 5.
This function is recursive.
 Return nil."
  (let ((last-left view)
        (index 0)
        (len (seq-length seq))
        this)
    (while (< index len)
      (setq this (seq-elt seq index))
      (cl-incf index)
      (ggui-put-after this last-left)
      (setq last-left this)))
  nil)

;; view to seq
(cl-defmethod ggui-put-before ((view ggui-view) (seq sequence))
  "Put VIEW before the first element of SEQ.
Return nil."
  (ggui-put-before view (seq-elt seq 0))
  nil)

(cl-defmethod ggui-put-after ((view ggui-view) (seq sequence))
  "Put VIEW after the last element of SEQ.
Return nil."
  (ggui-put-after view (seq-elt seq (1- (seq-length seq))))
  nil)

;; seq to seq
(cl-defmethod ggui-put-before ((seq1 sequence) (seq2 sequence))
  "Put VIEW before the first element of SEQ.
Return nil."
  (ggui-put-before seq1 (seq-elt seq2 0))
  nil)

(cl-defmethod ggui-put-after ((seq1 sequence) (seq2 sequence))
  "Put VIEW after the last element of SEQ.
Return nil."
  (ggui-put-after seq1 (seq-elt seq2 (1- (seq-length seq2))))
  nil)

(cl-defmethod ggui--remove-display ((seq sequence))
  "Remove display of SEQ.
This function is recursive."
  (seq-map #'ggui--remove-display seq))

(cl-defmethod ggui-put-before :before ((seq sequence) (_ ggui-view))
  "Remove AVIEW's display before adding it."
  (ggui--remove-display seq))

(cl-defmethod ggui-put-after :before ((seq sequence) (_ ggui-view))
  "Remove AVIEW's display before adding it."
  (ggui--remove-display seq))


;;;; App
;;
;; The pointer to current app and page is stored in
;; frame parameters.
;;
;; Developers define subclasses of `ggui-app' and `ggui-page'
;; for their app and each page of the app.
;; They should share a common prefix.
;;
;; An app creates instances of all pages on startup.
;; the other setup (buffers, windows, etc) take place
;; when a page segue to another page.

;;;;; Class

(defvar ggui-app-list ()
  "A list of existed names.")

(defvar ggui-ask-before-quit t
  "If non-nil, ask the user before quit an app.")

(ggui-defclass ggui-app ()
  ((name
    :type string
    :documentation "The name of the app.
It has to be unique because it is used to name buffers, etc.
Subclasses should set a default name.")
   (page-alist
    :type list
    :documentation "An alist of created pages.")
   (biggiebuffer
    :type buffer
    :documentation "Each app has a single biggie buffer.")
   (hint-buffer
    :type buffer
    :documentation "Each app has a single hint buffer.")
   (frame
    :type frame
    :documentation "The frame used by the app."))
  "An app.")

(ggui-defclass ggui-page ()
  ((app
    :type ggui-app
    :documentation "The `ggui-app' this page belongs to.")
   (buffer-alist
    :type list
    :documentation "Buffers managed by this page.")
   (window-conf
    :type window-configuration
    :documentation "The window configuration."))
  "A page is a UI unit in the timeline, it keeps tracks of states.
Users can segue between pages.")

;;;;; Command

(defun ggui-hide-app ()
  "Hide the current app."
  (interactive)
  (ggui--hide-app (frame-parameter nil 'ggui-app)))

(defun ggui-quit-app ()
  "Quit current app."
  (interactive)
  (let ((app (frame-parameter nil 'ggui-app)))
    (when (or (not ggui-ask-before-quit)
              (y-or-n-p (format "Quit %s? " (ggui--name app))))
      (ggui--quit-app app))))


;;;;; Generic
;;
;; generic functions that apps and pages need to implement

;; what should a `ggui-open-app' implementation do:
;;
;; 1. set `frame' slot
;; 2. get a unique and set to `name' slot (see `ggui-get-unique-name')
;; 3. open a new frame and set frame parameter `ggui-app' to this app
;; 4. segue to a page.
;; biggie and hint buffer are setup automatically (see below)

;; If your app is a single-instance app, there should be a
;; command with the same name of the app as the entrance point
;; If your app is a multi-instance app, there should be a
;; switch command and a creation command, or you can combine them


;; open function should handle two cases:
;; 1. after user quit the app (or the app never opened)
;; 2. after user hide the app
(cl-defgeneric ggui--open-app ((app ggui-app))
  "Open APP.")

(cl-defmethod ggui--open-app :after ((app ggui-app))
  "Setup hint and biggie buffer."
  (setf (ggui--hint-buffer app) (ggui-make-hint-buffer (format " *hint-%s*" (ggui--name app))))
  (setf (ggui--biggiebuffer app) (generate-new-buffer (format " *biggie-%s*"(ggui--name app)))))

(cl-defgeneric ggui--hide-app ((app ggui-app))
  "Hide (but not quit) APP.")

(cl-defgeneric ggui--quit-app ((app ggui-app))
  "Quit APP.")

;; what should `initialize-instance' of an app do:
;;
;; create pages, set their `app' slot, and put into `page-alist'

;; what should `initialize-instance' of a page do:
;;
;; Setup buffers and add them to `buffer-alist'.

;; Get a unique name base on information in `ggui-app-list'
;; and other information, e.g., current directory, etc.
;; default implementation simply increment a counter.
;; Set the name to `name' slot
(cl-defgeneric ggui-get-unique-name ((app ggui-app))
  "Get a unique name and set it to `name' slot."
  (let ((name (ggui--name app))
        (num 1))
    (while (seq-find (lambda (app) (equal name (ggui--name app))) ggui-app-list)
      (setq name (format "%s<%d>" name num))
      (cl-incf num))
    (setf (ggui--name app) name)))

;; what should a `ggui-segue' implementation do:
;;
;; when segue in, setup window configuration and save to `window-conf' slot;
;; setup all the buffers and major modes. Setting frame parameter `ggui-page'
;; to this page is automatically handled.
;;
;; when segue out, clean up buffers and set `window-conf' slot again.
;;
;; The primary method is always defined by the TO page.
;; FROM page doesn't do anything except clean up in :after method
;; use :after so the TO page can get information from FROM page
;; during setup
;;
;; the entrance page of an app should implement a special
;; `ggui-segue' that accepts FROM to be nil, for obvious reason
(cl-defgeneric ggui-segue ((from ggui-page) (to ggui-page))
  "Segue from FROM page to TO page.")

(cl-defmethod ggui-segue :after (_ (to ggui-page))
  (set-frame-parameter nil 'ggui-page to))

(defun ggui-segue-to (page)
  "Segue from current page to PAGE.
PAGE is a symbol representing the page in app's `ggui--page-alist'."
  (ggui-segue (frame-parameter nil 'ggui-page)
              (alist-get page (ggui--page-alist (frame-parameter nil 'ggui-app)))))

;;;;; Helpers

(defun ggui--this-app ()
  "Get current frame's app"
  (or (frame-parameter nil 'ggui-app)
      (signal 'ggui-app-missing)))

(defun ggui--this-page ()
  "Get current frame's current page."
  (or (frame-parameter nil 'ggui-page)
      (signal 'ggui-page-missing)))

;;;; Biggiebuffer
;;
;; each app has a single biggiebuffer that shares history

;;;;; Variable, command, minor mode

(defvar-local ggui-biggie-history nil
  "A list of user input history of biggiebuffer.")

(defvar-local ggui-biggie-finish-fn nil
  "The finish function used in `ggui-biggie-finish'.")

(defvar-local ggui-biggie-abort-fn nil
  "The abort function used in `ggui-biggie-abort'.")

(defvar-local ggui-biggie-update-fn nil
  "The update function used in `ggui-biggie-update'.")

(defvar ggui-biggie-pop-fn nil ;; TODO defualt pop function
  "Functions used to display biggiebuffer.
Takes one argument, the biggiebuffer, and returns the window
No assumptions about the position of the point.")

(define-minor-mode ggui-biggie-mode
  "Minor mode of biggiebuffer."
  :lighter " BIGGIE"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ggui-biggie-finish)
            (define-key map (kbd "C-c C-k") #'ggui-biggie-abort)
            map))

(define-minor-mode ggui-biggie-update-mode
  "In this mode biggiebuffer updates after every command."
  :lighter ""
  (if ggui-biggie-update-mode
      (add-hook 'after-change-functions #'ggui-biggie-update t t)
    (remove-hook 'after-change-functions #'ggui-biggie-update t)))

(defun ggui-biggie-finish ()
  "Finish with current content."
  (interactive)
  (ggui-biggie-update-mode -1)
  (add-to-list 'ggui-biggie-history (buffer-string))
  (if ggui-biggie-finish-fn
      (funcall ggui-biggie-finish-fn (buffer-string))
    (ggui-log 'warn "No `ggui-biggie-finish-fn' specified.")))

(defun ggui-biggie-abort ()
  "Abort with current content."
  (interactive)
  (ggui-biggie-update-mode -1)
  (if ggui-biggie-abort-fn
      (funcall ggui-biggie-abort-fn (buffer-string))
    (ggui-log 'warn "No `ggui-biggie-abort-fn' specified.")))

(defun ggui-biggie-update (_ _1 _2)
  "Update with current content."
  (if ggui-biggie-update-fn
      (fucnall ggui-biggie-update-fn (buffer-string))
    (ggui-log 'warn "No `ggui-biggie-update-fn' specified.")))

;;;;; Internal API

(defun ggui--biggiebuffer ()
  "Return the biggie-buffer of current app (of the selected frame)."
  (ggui--biggiebuffer (frame-parameter nil 'ggui-app)))

(defun ggui-invoke-biggie (finish-callback abort-callback &optional update-callback)
  "Invoke biggiebuffer with FINISH-CALLBACK and ABORT-CALLBACK.
FINISH-CALLBACK is called with biggiebuffer's content when the user finish,
ABORT-CALLBACK is called with biggiebuffer's content when the user abort.
If non-nil, UPDATE-CALLBACK will be called with biggiebuffer's content
whenever biggiebuffer's content has changed.

This function should be called inside other functions for a user input."
  (select-window (ggui-pop-biggie (frame-parameter nil 'ggui-page)))
  (if (eq (current-buffer) (ggui--buggiebuffer (frame-parameter nil 'ggui-app)))
      (progn
        (setq ggui-biggie-finish-fn finish-callback
              ggui-biggie-abort-fn abort-callback
              ggui-biggie-update-fn update-callback)
        (if update-callback
            (ggui-biggie-update-mode)
          (ggui-biggie-update-mode -1)))))

(cl-defgeneric ggui-pop-biggie ((page ggui-page))
  "Pop a buggiebuffer. Specific location of the window and size
depends on each PAGE. By default use a side window.
Return the window used for displaying biggiebuffer,
nil if no suitable window can be found."
  (display-buffer-in-side-window (ggui--biggiebuffer) '((side . bottom))))

;;;; Hint buffer
;; Hint buffer displays the available bindings and
;; other instructions.
;;
;; Each app has one hint buffer.
;;
;; Hint buffer has two `ggui-view's:
;; doc and binding. Doc is above binding.
;; Binding shows available bindings and doc shows extra
;; instructions.
;;
;;      hint buffer                  ggui-map:
;; +-------------------------------+
;; |  Some insructions             | documentation (doc) (setf (ggui--hint-doc (ggui--this-app)))
;; |                               |
;; |  C-c C-c Finish               | keybinding (map) (setf (ggui--hint-binding (ggui--this-app)))
;; |  C-c C-k Abort                |
;; |  ...                          |
;; |                               |
;; |                               |
;; |                               |
;; |                               |
;; |                               |
;; |                               |
;; |                               |
;; +-------------------------------+

;;;;; Variable

(defvar ggui-hint-pop-fn nil ; TODO default pop function
  "Function used to show hint buffer.

Although it is supposes to always visible in an app,
developers might want to hide hint buffer.
Similar to `ggui-biggie-pop-fn', this function takes the hint buffer
as argument and returns the window it is displayed in.
No assumptions about the position of the point.")

;;;;; Buffer

(defvar-local ggui--hint-doc nil
  "A `gguiview'. The instruction part of hint buffer..")

(defvar-local ggui--hint-binding nil
  "A `gguiview'. The binding part of hint buffer.")

(defun ggui-make-hint-buffer (name)
  "Return a hint buffer with NAME."
  (let ((buffer (ggui--setup-buffer (generate-new-buffer name))))
    (with-current-buffer buffer
      (ggui-put-after (setq ggui--hint-doc (ggui-view-new ""))
                      ggui--top-view)
      (ggui-put-before (setq ggui--hint-binding (ggui-view-new ""))
                       ggui--bottom-view))))

;;;;; Virtual slot of ggui-app

(cl-defmethod (setf ggui--hint-doc) (instruction (app ggui-app))
  "Display INSTRUCTION in the doc part of the hint buffer of APP.

Error: `ggui-app-misssing'."
  (with-current-buffer (ggui--hint-buffer app)
    (setf (ggui--text ggui--hint-doc) instruction)))

(cl-defmethod ggui--hint-doc ((app ggui-app))
  "Get the instruction in the doc part of the hint buffer of APP."
  (with-current-buffer (ggui--hint-buffer app)
    (ggui--text ggui--hint-doc)))

(cl-defmethod (setf ggui--hint-binding) (hint (app ggui-app))
  "Display HINT in the binding part of the hint buffer of APP.

Error: `ggui-app-misssing'."
  (with-current-buffer (ggui--hint-buffer app)
    (setf (ggui--text ggui--hint-binding) hint)))

(cl-defmethod ggui--hint-binding ((app ggui-app))
  "Get the binding hint in the binding part of the hint buffer of APP."
  (with-current-buffer (ggui--hint-buffer app)
    (ggui--text ggui--hint-binding)))


;;;; ggui map
;;
;; Primarily used by hint buffer

(cl-defstruct ggui-map
  "A keymap with a explanatory doc."
  map ; a keymap
  doc ; documentation string displayed above key binding information
  )

;;;;; Userland

(defvar ggui-binding-pad "  "
  "Padding used between columns of binding hints.")

(defvar ggui-binding-pad-between " -> "
  "Padding between key and definition of a binding in hint buffer.")

(defun ggui-use-map (gmap)
  "Activate `ggui-map' GMAP in the current buffer and display hint.
This map only activates for one command."
  (set-transient-map (ggui-map-map gmap))
  (ggui--display-map-hint gmap))

(defun ggui-use-map-default (gmap)
  "Activate `ggui-map' GMAP in the current buffer and display hint.

Unlike `ggui-use-map', MAP is persistent."
  (use-local-map (ggui-map-map gmap))
  (ggui--display-map-hint gmap))

(defun ggui-define-map (doc &rest binding-list)
  "Return a `ggui-map' with BINDING-LIST and DOC.
DOC is the full description,
and BINDING-LIST are key bindings.

BINDING-LIST looks like

    KEY DEFINITION
    KEY DEFINITION

KEY is passed to `kbd' before use.

as in `define-key'. It is recommended to add a short description:

    KEY '(\"DESCRIPTION\" . DEFINITION)
    KEY '(\"DESCRIPTION\" . DEFINITION)

so the hint buffer can show a more friendly description of the command.
If you don't add description, `ggui-use-map' will use the command name as the description."
  (make-ggui-map :doc doc :map (let ((map (make-sparse-keymap)))
                                 (while binding-list
                                   (let ((key (pop binding-list))
                                         (def (pop binding-list)))
                                     (define-key map (kbd key) def)))
                                 map)))

;;;;; Backstage

(defun ggui--display-map-hint (gmap)
  "Display `ggui-map' GMAP's documentation and bindings in hint buffer."
  (setf (ggui--hint-doc (ggui--this-app)) (ggui-map-doc gmap))
  (setf (ggui--hint-binding (ggui--this-app)) (ggui--map-to-hint gmap)))

(defsubst ggui--hint-len (hint)
  "Return the length of HINT.

Example of HINT: (\"C-c C-c\" . \"Finish\")."
  (+ (ggui--visual-length (car hint))
     (ggui--visual-length (cdr hint))))

(defun ggui--map-to-hint (map window)
  "Translate MAP to a string that hint buffer can display.
WINDOW is the window of hint buffer, it is needed for its dimensions."
  (let* ((text-lst (ggui--map-to-text map))
         (window-width (window-width window))
         (window-height (window-height window))
         ;; average length of text (C-c C-c Finish), possibly float
         (avg-len (/ (cl-reduce (lambda (a b)
                                  (+ a (ggui--hint-len b)))
                                text-lst
                                :initial-value 0)
                     (length text-lst)))
         (binding-pad-len (ggui--visual-length ggui-binding-pad))
         (binding-pad-between-len (ggui--visual-length ggui-binding-pad-between))
         (avg-len-with-padding (+ avg-len
                                  binding-pad-len
                                  binding-pad-between-len))
         (column-num (floor (/ (float window-width) avg-len-with-padding)))
         (row-num (ceiling (/ (float (length text-lst)) column-num)))
         ;; each element is a list of hint entries(cons),
         ;; each list represents a column
         (text-matrix (cl-loop for x from 0 to (1- column-num)
                               collect ()))
         ;; max length of the binding text
         ;; based on column number and window width
         (max-binding-len (floor (/ (- (float window-width)
                                       (* (1- column-num) binding-pad-len)
                                       (* column-num binding-pad-between-len))
                                    column-num)))
         final-lst) ; final 1D list to concat into a string
    ;; setup text-matrix
    ;;
    ;; scratch:
    ;; (setq from '(0 1 2 3 4 5 6 7 8 9))
    ;; (setq to (list () ()))
    ;; (cl-loop for iter-count from 0 to 4
    ;;          do (cl-loop for column-offset from 0 to 1
    ;;                      do  (push (nth (+ (* iter-count 2) column-offset)
    ;;                                     from)
    ;;                                (nth column-offset
    ;;                                     to))))
    ;; to
    ;; ((8 6 4 2 0) (9 7 5 3 1))
    (cl-loop for iter-count from 0 to (1- row-num)
             do (cl-loop for column-offset from 0 to (1- column-num) ; width of each segment
                         do  (ggui--push-end (or (nth (+ (* iter-count column-num) ; start of each segment
                                                         column-offset) ; offset in each segment
                                                      text-lst) ; from
                                                 (cons nil nil))
                                             (nth column-offset
                                                  text-matrix)))) ; to
    (print text-matrix)
    ;; pad keys and definitions for each column
    (dolist (column text-matrix)
      (let* ((max-key-len (cl-reduce
                           (lambda (a hint) (max a (ggui--visual-length (car hint))))
                           column
                           :initial-value 0))
             (max-def-len (- max-binding-len max-key-len)))
        ;; pad key base on max length
        (mapc (lambda (hint)
                (setf (car hint)
                      (let ((key (car hint)))
                        (if key
                            (concat (make-string (- max-key-len (ggui--visual-length key))
                                                 ?\s)
                                    key)
                          (make-string max-key-len ?\s)))))
              column)
        ;; pad definition base on max-def-len
        (mapc (lambda (hint)
                (setf (cdr hint)
                      (let ((binding (cdr hint)))
                        (if binding
                            (concat (substring binding 0 (min max-def-len
                                                              (ggui--visual-length binding)))
                                    (make-string (max 0 (- max-def-len (ggui--visual-length binding)))
                                                 ?\s))
                          (make-string max-def-len ?\s)))))
              column)))
    ;; generate string
    (cl-loop
     for row-idx from 0 to (1- row-num)
     ;; nth row in each column
     do (cl-loop
         for column-idx from 0 to (1- column-num)
         do (let ((hint (nth row-idx (nth column-idx text-matrix))))
              (ggui--push-end (concat (car hint)
                                      ;; pad between is already added in map-to-text
                                      (cdr hint)
                                      (if (eq column-idx (1- column-num))
                                          ;; is this the last column in the row?
                                          "\n"
                                        ggui-binding-pad))
                              final-lst))))
    ;; concat everything together
    (apply #'concat final-lst)))

(defun ggui--map-to-text (map)
  "Generate a list of binding text entries from MAP recursively.

Each entry is a cons: (key . definition), both are string.

This is primarily used by `ggui-map', which are small maps
(bindings to other `ggui-map's are lambdas instead of other keymaps),
so it is recursive (unlike which-key). You might not want to use it on
large (deep) keymaps."
  (let (key ; type
        def ; binding
        name ; item-name
        help ; help-string
        def-lst)
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Format-of-Keymaps.html
    ;;
    (setq def-lst
          (if (or (not (listp map))
                  (eq 'menu-bar (car map)))
              ;; this function doesn't handle the case
              ;; when MAP is a char-table, vector or a string
              ;; and when MAP is a menu bar map
              (setq def-lst ())
            ;; handles
            (if (eq (car map) 'keymap)
                ;; top level keymaps don't have key
                (progn (setq key nil)
                       (setq def map))
              (setq key (car map))
              (setq def (cdr map)))
            ;; stringify KEY, it should be either a key or ""
            (setq key (if key
                          (key-description (vector key))
                        ""))
            ;; stringify DEF
            ;;
            ;; example of DEF: ("Abort" function ggui-biggie-abort)
            ;; or C-k & C-c: (keymap (11 "Abort" function ggui-biggie-abort)
            ;;                       (3 "Finish" function ggui-biggie-finish))
            (cond
             ;; 1. DEF is a keymap
             ((and (listp def)
                   (eq 'keymap (car def)))
              ;; set def-lst to:
              (apply 'append (mapcar #'ggui--map-to-text
                                     (cdr def))))
             ;; 2. DEF is a extended menu
             ((and (listp def)
                   (eq 'menu-item (car def)))
              ;; set def-lst to:
              (list (cons "" "menu")))
             ;; 3. DEF is a ordinary binding
             ;; set def-lst to:
             (t (list
                 (cons "" ; key
                       (propertize
                        (cond
                         ;; 3.1 def = lambda
                         ((and (listp def)
                               (functionp def))
                          (setq help (pp-to-string def))
                          "lambda")
                         ;; 3.2 def = (name . func-symbol-or-lambda) or (name help . func-symbol-or-lambda)
                         ((and def (listp def)) ; in case def = nil
                          (progn (setq name (let ((name (nth 0 def))) (if (stringp name) name nil)))
                                 (setq help (let ((help (nth 1 def))) (if (stringp help) help nil)))
                                 name))
                         ;; 3.3 def = (function func-symbol) or (quote func-symbol)
                         ((and (symbolp def)
                               (functionp def))
                          (symbol-name def))
                         ;; 3.4 ???
                         (t "???")) ; TODO maybe error here
                        'help-echo help)))))))
    ;; cat key and (each) def
    (mapcar (lambda (def) (cons (concat key ; this key
                                        (if (equal (car def) "")
                                            ggui-binding-pad-between
                                          " ")
                                        (car def)) ; previous key
                                (cdr def)))
            def-lst)))

(defun ggui--gmap-to-hint (gmap)
  "Generate a binding hint (like which-key) from `ggui-map' GMAP."
  ;; Translate each binding to a text entry and gather into a list
  ()
  (let ((window-width (window-width))
        (entry-lst ()))
    ))

;;;; Provide

(provide 'ggui)

;;; ggui ends here
