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

;;;; Error

;; TODO what error condition does this error belong to?
(define-error 'ggui-pos-out-of-range "Position is out of the range of the buffer (pos showed after colon)" '(error))
(define-error 'ggui-buffer-missing "There is no such buffer" '(file-missing error))
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

;;;; buffer

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
         (setq ggui--setup t)))))
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


;;;; Provide

(provide 'ggui)

;;; ggui ends here
