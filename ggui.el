;;; ggui.el --- Grand GGUD User Interface      -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Yuan Fu.

;; Author: Yuan Fu <casouri@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; Keywords: extensions
;; Package-Requires: ((emacs ""))
;; Version: 0.1.0

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

(defvar ggui--log-level-plist '(:error 3 :warn 2 :info 1 :debug 0)
  "Plist used to know which level is more bad ass.")

(defun ggui-log (level str &rest args)
  "Same as `message'.
STR and ARGS are like those in format.
LEVEL is a symbol, can be :info :warn or :error."
  (if-let ((this-level (plist-get ggui--log-level-plist level))
           (min-level (plist-get ggui--log-level-plist ggui-log-level)))
      (when (>= this-level ; this log is more bad ass than setting
                min-level)
        (apply #'message
               (format "GGUI %s %s: %s"
                       (format-time-string "%Y-%m-%d %T" (current-time))
                       (plist-get '(:warn "Warn" :error "Error" :info "Info") level)
                       str)
               args))
    (signal 'ggui-invalid-argument (list "LEVEL is not one among `ggui-log-level-plist'"))))

(defmacro ggui--push-end (elt lst)
  "Push ELT to the end of LST."
  ;; (setq ll (list 1 2 3))
  ;; (benchmark-run 1000 (ggui--push-end 0 ll))
  ;; (0.099558 1 0.0901969999999892)
  `(setf ,lst (append ,lst (list ,elt))))

(defun ggui--fix-len-visual (str len &optional pad)
  "Trim STR to length LEN.

LEN must be a visual length, i.e. returned by `ggui--visual-length'.

If PAD is non-nil, PAD STR with space when STR is shorter than LEN.
Otherwise (PAD nil) just return STR.
IF PAD is 'right, pad on right, if it is 'left, pad on left.

Visual means two things:
1. length is measured in visual way, i.e., CJK chars occupy two units.
2. \"..\" is added to the end of STR when chopping
unless LEN is less than 2.

If STR is shorter than LEN, spaces are padded on the SIDE."
  (let ((real-len (ggui--visual-length str)))
    (if (> real-len len)
        (if (> len 2)
            (concat (ggui--visual-substring str 0 (- len 2)) "..")
          (ggui--visual-substring str 0 len))
      (if pad
          (let ((padding (make-string (- len real-len) ?\s)))
            (pcase pad
              ('right (concat str padding))
              ('left (concat padding str))
              (_ (signal 'ggui-invalid-argument "PAD should be either 'left or 'right, you passed %s" pad))))
        str))))

(defun ggui--buffer-window (buffer &optional frame)
  "Return the first window found in FRAME that displays BUFFER.
current frame is used when FRAME is nil.

Return nil when no window is found."
  (seq-find (lambda (wind) (eq (window-buffer wind) buffer)) (window-list frame nil)))

(defun ggui--find-index (elt lst &optional compare-fn)
  "Find ELT's position in LST and return it.
Compare with COMPARE-FN, if it's nil, use `eq'.

Return nil if no match is found."
  (if lst (catch 'found
            (let ((idx 0))
              (seq-doseq (elm lst)
                (when (funcall (or compare-fn #'eq) elt elm)
                  (throw 'found idx))
                (cl-incf idx))
              nil))
    (signal 'wrong-type-argument (list "LST is nil"))))

(defmacro ggui-seq-last (seq)
  "The last element of SEQ."
  `(seq-elt ,seq (1- (seq-length ,seq))))

(defun ggui--erase-buffer ()
  "Erase current buffer and cleans up."
  (erase-buffer)
  (remove-overlays))

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
If TO or FROM is in the middle of a CJK char,
throw away the char and append a space.

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

;;;; Global hook

(defvar ggui--after-buffer-change-hook ()
  "This hook runs after buffer changes.
Each function in the hook is called with `last-buffer' and this-buffer.")

(defvar ggui--last-buffer nil
  "Used by `ggui--after-buffer-change-hook'.")

(defun ggui--maybe-run-after-buffer-change-hook ()
  "As the name suggest."
  (let ((current-buffer (current-buffer)))
    (unless (eq ggui--last-buffer current-buffer)
      (run-hook-with-args 'ggui--after-buffer-change-hook
                          ggui--last-buffer current-buffer)
      (setq ggui--last-buffer current-buffer))))

(add-hook 'post-command-hook #'ggui--maybe-run-after-buffer-change-hook)

;;;; Error

;; TODO what error condition does this error belong to?
(define-error 'ggui-pos-out-of-range "Position is out of the range of the buffer (pos showed after colon)" '(error))
(define-error 'ggui-buffer-missing "There is no such buffer" '(file-missing error))
(define-error 'ggui-app-missing "There is no app." '(error))
(define-error 'ggui-end-of-line "The line is not long enough" '(end-of-buffer error))
(define-error 'ggui-delimiter-misplace "A ggui delimiter should/should not be here." '(error))
(define-error 'ggui-prohibit-edit "This edition is not allowed" '(error))
(define-error 'ggui-view-not-present "This view is not on any buffer." '(error))
(define-error 'ggui-element-missing "The element is assumed to be in a list, but it is not" '(error))
(define-error 'ggui-invalid-argument "The argument is invalid" '(error))


;;;; ggui-view

;;;;; Class & init & helper

(ggui-defclass ggui-view ()
  ((overlay
    :type (satisfies overlayp)
    :documentation "Private. The overlay covering the range.
Don't use `slot-value' on this, instead, use the accessor `ggui--overlay'.")
   (text
    :type string
    :documentation "Public. The text of the view.
It cannot be a empty string, otherwise delimiter checkers error when inserting the view.")
   (property-list
    :documentation "Dummy slot used by init: (PROP VALUE PROP VALUE).
DO NOT USE THIS SLOT!
Use `ggui--overlay-put' to add properties to overlay."))
  ;; It is covered by an overlay (slot), and must have line feed and after it (which is included).
  "A piece of overlay-managed text.
Public slot: text
Virtual slot:
- beg           R&W
- end           R&W
- buffer        R
- property-list R&W")

(cl-defmethod cl-print-object ((view ggui-view) stream)
  (princ (format "{ ggui-view OVERLAY: %S TEXT: %s }"
                 (ggui--overlay view)
                 (ggui--text view))
         stream))

(cl-defmethod initialize-instance :after ((view ggui-view) &rest _)
  "Setup overlay and property."
  (unless (ggui--overlay view)
    (setf (ggui--overlay view)
          (apply #'ggui--make-overlay
                 1 1 nil
                 (ggui--property-list view))))
  (overlay-put (ggui--overlay view) 'ggui-view view)
  (slot-makeunbound view 'property-list))

(defun ggui-view-new (text &rest property-list)
  "Return a `ggui-view' object with TEXT.

PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (ggui-view :text text :property-list property-list))

(cl-defmethod ggui--presentp ((view ggui-view))
  "Return t if VIEW is inserted into some buffer (present), nil if not."
  ;; if the overlay is not in any buffer, VIEW is not present
  (if (overlay-buffer (ggui--overlay view)) t nil))

(cl-defmethod ggui--presentp ((seq sequence))
  (seq-reduce (lambda (a b) (and a b))
              (seq-map #'ggui--presentp seq)
              t))

(defun ggui--make-overlay (beg end &optional buffer &rest property-list)
  "Return an overlay from BEG to END in BUFFER.

Don’t bind overlay to any buffer if BUFFER is nil.
PROPERTY-LIST:
Remaining arguments form a sequence of PROPERTY VALUE pairs
for overlay properties to add to the result."
  (let ((overlay (make-overlay beg end buffer t nil)))
    ;; marker: insert in front: included, end: included
    (while property-list
      (overlay-put overlay (pop property-list) (pop property-list)))
    ;; don’t bind overlay to any buffer if BUFFER is nil.
    (unless buffer (delete-overlay overlay))
    overlay))

(cl-defmethod (setf ggui--text) :after (text (view ggui-view))
  "Update display in buffer."
  (when (ggui--presentp view)
    (with-current-buffer (ggui--buffer view)
      (ggui--edit
       (let ((beg (ggui--beg view)))
         (goto-char beg)
         (delete-region beg (ggui--end view))
         (insert text)
         (ggui--move-overlay view beg (point)))))))

;;;;; Beg & end & buffer

(cl-defmethod ggui--beg-mark ((view ggui-view))
  "Return the beginning of VIEW as a marker.
Don't forget to delete the marker after using them!
They are very expensive."
  (let ((overlay (ggui--overlay view)))
    (set-marker (make-marker) (overlay-start overlay) (overlay-buffer overlay))))

(cl-defmethod ggui--end-mark ((view ggui-view))
  "Return the end of VIEW as a marker.
Don't forget to delete the marker after using them!
They are very expensive."
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
Return the overlay."
  ;; basically create a new one, set values, delete the old one
  (move-overlay (ggui--overlay view)
                (or beg (ggui--beg view))
                (or end (ggui--end view))
                buffer))

;;;;; Put/insert before/after

;;;;;; Generic
;;
;; Implement at least ggui-put-before/after for
;; generic views you want to support.

(cl-defgeneric ggui-put-before (a b)
  "Insert A before B.
Return A.")

(cl-defgeneric ggui-put-after (a b)
  "Insert A after B.
Return A.")


;;;;;; Check

;; simply to please compiler
;; real definition below in "managed buffer" section
(defvar ggui-top-view)
(defvar ggui-bottom-view)

(cl-defmethod ggui-put-before :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui-top-view view)
    (signal 'ggui-prohibit-edit (list (format "Nothing can be put before a `ggui-top-view', you try to put: %s" stuff))))
  (unless (ggui--presentp view)
    (signal 'ggui-view-not-present (list (format "%s is not present yet, you can't put STUFF before it" view)))))

(cl-defmethod ggui-put-after :before (stuff (view ggui-view))
  "Check for misuse."
  (when (eq ggui-bottom-view view)
    (signal 'ggui-prohibit-edit (list (format "Nothing can be put after a `ggui-bottom-view', you try to put: %s" stuff))))
  (unless (ggui--presentp view)
    (signal 'ggui-view-not-present (list (format "%s is not present yet, you can't put STUFF before it" view)))))

(cl-defmethod ggui-put-before :before (_ stuff)
  "Check for misues."
  (unless stuff (signal 'wrong-type-argument (list "STUFF cannot be nil."))))

(cl-defmethod ggui-put-after :before (_ stuff)
  "Check for misues."
  (unless stuff (signal 'wrong-type-argument (list "STUFF cannot be nil."))))

;;;;;; View

(cl-defmethod ggui-put-before ((aview ggui-view) (view ggui-view))
  (ggui--edit
   (let (abeg aend)
     (goto-char (ggui--beg view))
     (setq abeg (point))
     (insert (ggui--text aview))
     (setq aend (point))
     (ggui--move-overlay aview abeg aend)))
  aview)

(cl-defmethod ggui-put-after ((aview ggui-view) (view ggui-view))
  (ggui--edit
   (let (abeg aend)
     (goto-char (ggui--end view))
     (setq abeg (point))
     (insert (ggui--text aview))
     (setq aend (point))
     (ggui--move-overlay aview abeg aend)))
  aview)

;;;;;; Remove before insert

(cl-defmethod ggui-put-before :before ((aview ggui-view) _)
  "Remove AVIEW's display before adding it."
  (when (ggui--presentp aview) (ggui--remove-display aview)))

(cl-defmethod ggui-put-after :before ((aview ggui-view) _)
  "Remove AVIEW's display before adding it."
  (when (ggui--presentp aview) (ggui--remove-display aview)))

;;;;; Remove
(cl-defmethod ggui--remove-display ((view ggui-view))
  "Remove VIEW's presence from buffer."
  (when (ggui--presentp view)
    (ggui--edit
     (delete-region (ggui--beg view) (ggui--end view)))))

;;;; ggui managed buffer
;;
;; One such buffer has a top and a bottom anchor
;; to insert `ggui-view's and is read only.

(defvar-local ggui--setup nil
  "Whether current buffer is setup.")
(defvar-local ggui-top-view nil
  "The top most (invisible) view of buffer.")
(defvar-local ggui-bottom-view nil
  "The bottom most (invisible) view of buffer.")

(defun ggui--setup-buffer (buffer &optional force)
  "Setup BUFFER and return it. BUFFER can be either a string or a buffer.
If it is a string and no corresponding buffer is present, create one.
If buffer already exists, it is erased.

If the buffer is already setup, do nothing.
However, if FORCE is t, set it up it anyway."
  ;; you probably don't want to default buffer to be current buffer
  ;; when the function might erases buffer
  (let ((buffer (get-buffer-create buffer)))
    (unless (buffer-live-p buffer)
      (signal 'ggui-buffer-missing
              (list (format "BUFFER is not a live buffer: %s" buffer))))
    ;; setup
    (with-current-buffer buffer
      (if (and ggui--setup (not force))
          (ggui-log :warn "Buffer %S already set up but someone try to set up again." buffer)
        (ggui--edit
         (ggui--erase-buffer)
         (setq ggui-top-view (ggui-view :text "" :overlay (make-overlay 1 1 nil nil nil)))
         (setq ggui-bottom-view (ggui-view :text "" :overlay (make-overlay 1 1 nil t t)))
         (setq ggui--setup t)
         (read-only-mode))))
    buffer))

(defun ggui-generate-managed-buffer (name)
  "Return a new `ggui-view' managed buffer with name NAME.
NAME might be modified if some other buffer with the same name exists."
  (let ((buf (generate-new-buffer name)))
    (ggui--setup-buffer buf)
    buf))

;;;;; ggui-object-at-point

;; FIXME I don’t like the implementation
(defun ggui-object-at-point ()
  "Return the ggui object at point.
Return nil if none found."
  (interactive)
  (catch 'found
    (if ggui--setup
        (dolist (ov (overlays-at (point)))
          (when-let ((obj (plist-get (overlay-properties ov) 'ggui-view)))
            (throw 'found obj))))))

;;;; Toggleable

;;FIXME a better design?

(cl-defgeneric ggui-toggle-forward (_)
  "Toggle the object's state forward.")

(cl-defgeneric ggui-toggle-back (_)
  "Toggle the object's state backward.")

(cl-defgeneric ggui-toggle-to (_ state-index)
  "Toggle to the STATE-INDEX th state of the toggleable.")

(defun ggui-toggle-at-point (n)
  "Toggle the stuff at point (if it relates to a ggui object).
If provided with numeric prefix argument, toggle to state N."
  (interactive "p")
  (if-let ((obj (ggui-object-at-point)))
      (condition-case nil
          (ggui-toggle-to n)
        ('cl-no-applicable-method (message "Object at point doesn't support toggling")))
    (message "No object at point.")))

(ggui-defclass ggui-default-toggle ()
  ((state
    :type (integer 0 *)
    :initform 0
    :documentation "The state of the toggle, must >= 1")
   (state-count
    :type (integer 1 *)
    :initform 1
    :documentation "The number of states, must >= 1."))
  "Default toggle.")

(cl-defmethod ggui--normalize-state (state (toggle ggui-default-toggle))
  "Normalize STATE of TOGGLE.
Basically STATE < 0 --> max state
          STATE > max state --> 0"
  (cond ((< state 0)
         (1- (ggui--state-count toggle)))
        ((> state (ggui--state-count toggle))
         0)
        (t state)))

(cl-defmethod ggui-toggle-back ((toggle ggui-default-toggle))
  (ggui-toggle-to (setf (ggui--state toggle)
                        (ggui--normalize-state (1- (ggui--state toggle))))))

(cl-defmethod ggui-toggle-forward ((toggle ggui-default-toggle))
  (ggui-toggle-to (setf (ggui--state toggle)
                        (ggui--normalize-state (1+ (ggui--state toggle))))))

;;;; Hideshow

(cl-defgeneric ggui-hide (_)
  "Hide the display.")

(cl-defgeneric ggui-show (_)
  "Show the display.")

(cl-defmethod ggui-hide ((view ggui-view))
  (if (ggui--presentp view)
      (ggui--overlay-put view 'invisible t)
    (signal 'ggui-view-not-present nil)))

(cl-defmethod ggui-hide ((seq sequence))
  (seq-map #'ggui-hide seq))

(cl-defmethod ggui-show ((view ggui-view))
  (if (ggui--presentp view)
      (ggui--overlay-put view 'invisible nil)
    (signal 'ggui-view-not-present nil)))

(cl-defmethod ggui-show ((seq sequence))
  (seq-map #'ggui-show seq))

;;;; List
;;
;; Use the list manipulation functions below
;; to keep `ggui-view's in the list in sync
;; when editing lists.
;;
;; For other generic sequences,
;; implement these generic functions for sequence:
;;
;; ggui-remove
;; ggui-insert-at
;; seq-elt
;; seq-find
;; seq-length
;; (setf seq-elt)

;;;;; ggui generic list function

(cl-defgeneric ggui-remove (elt seq &optional compare-fn)
  "Return a seq with ELT removed from SEQ.

If COMPARE-fn is used to compare equality, default to `eq'."
  (let ((fn (or compare-fn #'eq)))
    (seq-remove (lambda (elm)
                  (funcall fn elm elt))
                seq)))

(defmacro ggui-remove-n (elt seq &optional compare-fn)
  "(setf SEQ (ggui-remove ELT SEQ COMPARE-FN))."
  `(setf ,seq (ggui-remove ,elt ,seq ,compare-fn)))

(cl-defgeneric ggui-insert-at (elt seq n)
  "Insert ELT into SEQ at Nth position and return SEQ.
This function is not destructive.")

(defmacro ggui-insert-at-n (elt seq n)
  "(setf SEQ (ggui-insert-at ELT SEQ N))."
  `(setf ,seq (ggui-insert-at ,elt ,seq ,n)))


;;;;; Method implementation for list

(cl-defmethod ggui-insert-at :before (_ seq n)
  "Check"
  (when (or (< (seq-length seq) n)
            (< n 0))
    (signal 'args-out-of-range nil))
  (unless seq (signal 'wrong-type-argument (list "SEQ is nil"))))

(cl-defmethod ggui-insert-at (elt (seq list) n)
  (seq-concatenate 'list (seq-subseq seq 0 n)
                   (list elt)
                   (seq-subseq seq n (seq-length seq))))

;;;;; (Side effect) implementation for ggui-view

(cl-defmethod ggui-insert-at :after ((view ggui-view) (seq sequence) n)
  (let* ((len (seq-length seq)))
    (if (= n len)
        (ggui-put-after view (seq-elt seq (1- len)))
      (ggui-put-before view (seq-elt seq n)))))

(cl-defmethod ggui-remove :before ((view ggui-view) (seq sequence) &optional _)
  (if (seq-find (lambda (elt) (eq elt view)) seq)
      (ggui--remove-display view)))

;;;;; (Side effect) implementation for sequence (identical)

;; identical
(cl-defmethod ggui-insert-at :after ((view sequence) (seq sequence) n)
  ;; do side effect first so we don't worry about position N
  ;; changes when inserting VIEW
  (let* ((len (seq-length seq)))
    (if (= n len)
        (ggui-put-after view (seq-elt seq (1- len)))
      (ggui-put-before view (seq-elt seq n)))))

(cl-defmethod ggui-remove :before ((view sequence) (seq sequence) &optional _)
  (if (seq-find (lambda (elt) (eq elt view)) seq)
      (ggui--remove-display view)))

;;;;; Methods for ggui-view functions for list

;;;;;;; seq to view

(cl-defmethod ggui-put-before ((seq sequence) (view ggui-view))
  "Put every view in SEQ before VIEW, in normal order.
E.g., SEQ: (1 2 3 4) VIEW: 5 -> 1 2 3 4 5."
  (unless seq (signal 'wrong-type-argument '("SEQ is nil")))
  (ggui-put-before (seq-elt seq 0) view)
  (ggui-put-after (seq-subseq seq 1) (seq-elt seq 0))
  seq)

(cl-defmethod ggui-put-after ((seq sequence) (view ggui-view))
  "Put every view in SEQ after VIEW, in normal order.
E.g., SEQ: (2 3 4 5) VIEW: 1 -> 1 2 3 4 5."
  (unless seq (signal 'wrong-type-argument '("SEQ is nil")))
  (let ((last-left view)
        (index 0)
        (len (seq-length seq))
        this)
    (while (< index len)
      (setq this (seq-elt seq index))
      (cl-incf index)
      (ggui-put-after this last-left)
      (setq last-left this)))
  seq)

;;;;;;; view to seq

(defmacro ggui--check-nil (stuff)
  "Check STUFF is not nil.
If STUFF is nil, signal `wrong-type-argument'."
  `(unless ,stuff (signal 'wrong-type-argument ,(format "%s cannot be nil" (upcase (symbol-name stuff))))))

(cl-defmethod ggui-put-before ((view ggui-view) (seq sequence))
  "Put VIEW before the first element of SEQ."
  (ggui--check-nil seq)
  (ggui-put-before view (seq-elt seq 0))
  view)

(cl-defmethod ggui-put-after ((view ggui-view) (seq sequence))
  "Put VIEW after the last element of SEQ."
  (ggui--check-nil seq)
  (ggui-put-after view (ggui-seq-last seq))
  view)

;;;;;;; seq to seq

(cl-defmethod ggui-put-before ((seq1 sequence) (seq2 sequence))
  "Put VIEW before the first element of SEQ."
  (ggui--check-nil seq1)
  (ggui--check-nil seq2)
  (ggui-put-before seq1 (seq-elt seq2 0))
  seq1)

;; TODO move check into :before

(cl-defmethod ggui-put-after ((seq1 sequence) (seq2 sequence))
  "Put VIEW after the last element of SEQ."
  (ggui--check-nil seq1)
  (ggui--check-nil seq2)
  (ggui-put-after seq1 (seq-elt seq2 (1- (seq-length seq2))))
  seq1)

(cl-defmethod ggui--remove-display ((seq sequence))
  "Remove display of SEQ.
This function is recursive."
  (ggui--check-nil seq)
  (seq-map #'ggui--remove-display seq))

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

(cl-defmethod ggui--open-app :before ((app ggui-app))
  "Setup hint and biggie buffer."
  (setf (ggui--hint-buffer app) (ggui-make-hint-buffer (format " *hint-%s*" (ggui--name app))))
  (setf (ggui--biggiebuffer app) (ggui--make-biggie-buffer (ggui--name app))))

(cl-defgeneric ggui--hide-app ((app ggui-app))
  "Hide (but not quit) APP.")

;; You should set frame parameter `ggui-app' and `ggui-page' to nil
;; if you didn't create a new frame when opening the app.
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
  (let ((base-name (ggui--name app))
        (name (ggui--name app))
        (num 1))
    (while (seq-find (lambda (app) (equal name (ggui--name app))) ggui-app-list)
      (setq name (format "%s<%d>" base-name num))
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
;;
;; If you don't kill the buffers that the page creates when segueing out
;; better erase the buffer before writing to it when segueing in,
;;because you never know if the buffer has been setup or not.
(cl-defgeneric ggui-segue ((from ggui-page) (to ggui-page))
  "Segue from FROM page to TO page.")

(cl-defmethod ggui-segue :before (_ (to ggui-page))
  (set-frame-parameter nil 'ggui-page to))

(defun ggui-segue-to (page)
  "Segue from current page to PAGE.
PAGE is a symbol representing the page in app's `ggui--page-alist'."
  (ggui-segue (frame-parameter nil 'ggui-page)
              (alist-get page (ggui--page-alist (frame-parameter nil 'ggui-app)))))

;;;;; Helpers

(defmacro ggui-this-app ()
  "Get current frame's app.

Error: `ggui-app-missing'."
  `(or (frame-parameter nil 'ggui-app)
       (signal 'ggui-app-missing nil)))

(defmacro ggui-this-page ()
  "Get current frame's current page.

Error: `ggui-page-missing'."
  `(or (frame-parameter nil 'ggui-page)
       (signal 'ggui-page-missing nil)))


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

(defvar ggui-hint-buffer-mode-line-format nil
  "Mode line format of hint buffer.")

;;;;; Buffer

(defvar-local ggui--hint-doc nil
  "A `gguiview'. The instruction part of hint buffer..")

(defvar-local ggui--hint-binding nil
  "A `gguiview'. The binding part of hint buffer.")

(defun ggui-make-hint-buffer (name)
  "Return a hint buffer with NAME."
  (let ((buffer (ggui--setup-buffer (generate-new-buffer name))))
    (with-current-buffer buffer
      (setq mode-line-format ggui-hint-buffer-mode-line-format)
      (ggui-put-after (setq ggui--hint-doc (ggui-view-new " "))
                      ggui-top-view)
      (ggui-put-before (setq ggui--hint-binding (ggui-view-new " "))
                       ggui-bottom-view))
    buffer))

;;;;; pop hint (optional)

;; please compiler
(defvar ggui--default-hint)
(defvar ggui--local-map)

(defun ggui-show-hint (&optional buffer)
  "Display hint for current buffer or BUFFER.
Return the value of `ggui--default-hint.'"
  (with-current-buffer (or buffer (current-buffer))
    (if ggui--local-map
        (setq ggui--default-hint
              (ggui--display-map-hint ggui--local-map))
      (ggui-log :debug "Try to display hint but no map is set."))))

(cl-defgeneric ggui--ensure-hint ((page ggui-page))
  "Make sure there is a window displaying hint buffer.
Implementation can vary by PAGE.
Return that window, nil if PAGE doesn't want to (or can't) show hint."
  (ignore page)
  (if-let ((window (ggui--buffer-window (ggui--hint-buffer (ggui-this-app)))))
      window
    (display-buffer-in-side-window (get-buffer-create " tmp")
                                   '((side . bottom) (slot . 1))))) ; biggie is -1

;;;;; Default hint

(defvar-local ggui--default-hint '("" "")
  "Default hint of the buffer. It is a list like '(doc bindign).
Don't set this to nil.")
;; TODO if the hint window is not present when `use-map' is called,
;; default-hint will not be properly set.
;; And if you display that hint butter, no info will be there
;; properly handle that when redisplaying hint window

(defun ggui--hint-recover-default-hint (&optional _1 _2)
  ;; called with last buffer and this buffer in after buffer change hook
  "Recover the default hint for the current buffer."
  ;; run when:
  ;; buffer changes
  ;; a command in a `ggui-map' runs
  (when-let ((app (condition-case nil (ggui-this-app)
                    (ggui-app-missing nil))))
    (setf (ggui--hint-doc app)
          (nth 0 ggui--default-hint)
          (ggui--hint-binding app)
          (nth 1 ggui--default-hint))))

(add-hook 'ggui--after-buffer-change-hook #'ggui--hint-recover-default-hint)

;;;;; Virtual slot for ggui-app

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

(defvar ggui--local-map nil
  "Local `ggui-map'. Used to display hint when no default hint is set.")

(defun ggui-use-map (gmap)
  "Activate `ggui-map' GMAP in the current buffer and display hint.
This map only activates for one command."
  (set-transient-map (ggui-map-map gmap))
  (ggui--display-map-hint gmap))

(defun ggui-use-map-default (gmap)
  "Activate `ggui-map' GMAP in the current buffer and display hint.

Unlike `ggui-use-map', MAP remains persistent."
  (let ((my-map (copy-keymap (ggui-map-map gmap))))
    (when-let ((local-map (current-local-map)))
      (set-keymap-parent my-map local-map))
    (use-local-map my-map)
    (setq ggui--local-map my-map))
  (setq ggui--default-hint (ggui--display-map-hint gmap)))

(defun ggui-define-map (doc &rest binding-list)
  "Return a `ggui-map' with BINDING-LIST and DOC.
DOC is the full description,
and BINDING-LIST are key bindings.

Normally you want to add two newline in the end of DOC.

BINDING-LIST looks like

    KEY (ggui-map-binding ggui-map \"DESCRIPTION\" \"HELP\")

for `ggui-maps' and

    KEY (ggui-fn-binding func \"DESCRIPTION\" \"HELP\")

for other normal definitions (normal keymap, function, lambda, etc).

DESCRIPTION is a friendly name shown in hint buffer besides the key
HELP is a mouse hover tooltip.

You should only use `ggui-fn-binding
and `ggui-map-binding' in a `ggui-map' definition (this one).
Because they instrument the function(map) and do other things."
  (make-ggui-map :doc doc :map (let ((map (make-sparse-keymap)))
                                 (while binding-list
                                   (let ((key (pop binding-list))
                                         (def (pop binding-list)))
                                     (define-key map (kbd key) def)))
                                 map)))

(defun ggui-map-binding (map name &optional help)
  "Return a valid keymap binding.
It can be placed in the definition part in an element of a keymap.

MAP is a `ggui-map', it can be either symbol or the actual map.
NAME is the name displaced in hint buffer,
HELP is the optional help tooltip."
  (if help
      `(,name ,help . (lambda () (interactive) (ggui-use-map ,map)))
    `(,name . (lambda () (interactive) (ggui-use-map ,map)))))

(defun ggui-fn-binding (fn name &optional help)
  "Return a valid keymap binding.
It can be placed in the definition part in an element of a keymap.

FN is a function.
NAME is the name displaced in hint buffer,
HELP is the optional help tooltip."
  (if help
      `(,name ,help . (lambda () (interactive)
                        (ggui--hint-recover-default-hint)
                        (funcall #',fn)))
    `(,name . (lambda () (interactive)
                (ggui--hint-recover-default-hint)
                (funcall #',fn)))))

(cl-defmethod ggui-set-keymap-parent ((child ggui-map) (parent ggui-map))
  "Set CHILD's parent to PARENT.
Like `set-keymap-parent'."
  (set-keymap-parent (ggui-map-map child) (ggui-map-map parent)))

(cl-defmethod ggui-set-keymap-parent ((child ggui-map) parent)
  "Set CHILD's parent to PARENT.
Like `set-keymap-parent'."
  (set-keymap-parent (ggui-map-map child) parent))

(cl-defmethod ggui-set-keymap-parent (child (parent ggui-map))
  "Set CHILD's parent to PARENT.
Like `set-keymap-parent'."
  (set-keymap-parent child (ggui-map-map parent)))

;;;;; Backstage

(defun ggui--display-map-hint (gmap)
  "Display `ggui-map' GMAP's documentation and bindings in hint buffer.
Return list (doc hint)."
  (list (setf (ggui--hint-doc (ggui-this-app)) (ggui-map-doc gmap))
        (setf (ggui--hint-binding (ggui-this-app))
              (if-let ((window (ggui--ensure-hint (ggui-this-page))))
                  (ggui--map-to-hint (ggui-map-map gmap) window)
                (ggui-log :debug "Try to display hint but hint buffer is not on screen.")
                ""))))

(defun ggui--hint-len (hint)
  "Return the length of HINT.

Example of HINT: (\"C-c C-c\" . \"Finish\")."
  (+ (ggui--visual-length (car hint))
     (ggui--visual-length (cdr hint))))

(defun ggui--map-to-hint (map window)
  "Translate MAP to a string that hint buffer can display.
WINDOW is the window of hint buffer, it is needed for its dimensions."
  (let* ((text-lst (ggui--map-to-text map))
         (window-width (window-width window))
         ;; average length of text (C-c C-c Finish), possibly float
         (avg-len (/ (cl-reduce (lambda (a b)
                                  (+ a (ggui--hint-len b)))
                                text-lst
                                :initial-value 0)
                     (length text-lst)))
         (binding-pad-len (ggui--visual-length ggui-binding-pad))
         (binding-pad-between-len (ggui--visual-length ggui-binding-pad-between))
         (avg-len-with-padding (floor (+ avg-len
                                         binding-pad-len
                                         binding-pad-between-len)))
         (column-num (floor (/ (float window-width) avg-len-with-padding)))
         (row-num (ceiling (/ (float (length text-lst)) column-num)))
         ;; each element is a list of hint entries(cons),
         ;; each list represents a column
         (text-matrix (cl-loop for column-idx from 0 to (1- column-num)
                               with lst-len = (length text-lst)
                               with beg
                               with end
                               do (setq beg (+ (* column-idx row-num)))
                               do (setq end (+ beg row-num))
                               when (< beg lst-len)
                               collect (seq-subseq text-lst beg (min end lst-len))))
         ;; max length of the binding text
         ;; based on column number and window width
         (free-width (- window-width
                        (* (1- column-num) binding-pad-len)
                        (* column-num binding-pad-between-len)))
         ;; each element correspond to the max length of one column
         (max-binding-len-lst (cl-loop for column-idx from 0 to (1- column-num)
                                       collect (cl-reduce (lambda (a b)
                                                            (max a (ggui--hint-len b)))
                                                          (nth column-idx text-matrix)
                                                          :initial-value 0)))
         final-lst) ; final 1D list for concating into a string
    ;; calculate `max-binding-len' for each column
    ;;
    ;; while sum of the max lengths are wider than window width
    (while (> (cl-reduce #'+ max-binding-len-lst) free-width)
      ;; fing the index of the largest max len, reduce it by 1
      (let ((max-idx 0))
        (cl-loop for column-idx from 0 to (1- column-num)
                 do (when (> (nth column-idx max-binding-len-lst)
                             (nth max-idx max-binding-len-lst))
                      (setq max-idx column-idx)))
        (cl-decf (nth max-idx max-binding-len-lst))))
    ;; pad keys and definitions for each column
    (cl-loop
     for column-idx from 0 to (1- column-num)
     do (let* ((column (nth column-idx text-matrix))
               (max-binding-len (nth column-idx max-binding-len-lst))
               (max-key-len (cl-reduce
                             (lambda (a hint) (max a (ggui--visual-length (or (car hint) ""))))
                             column
                             :initial-value 0))
               (max-def-len (- max-binding-len max-key-len)))
          ;; pad key base on `max-key-len'
          (mapc (lambda (hint)
                  (setf (car hint)
                        (let ((key (car hint)))
                          (if key
                              (concat (make-string (- max-key-len (ggui--visual-length key))
                                                   ?\s)
                                      key)
                            (make-string max-key-len ?\s)))))
                column)
          ;; pad definition base on `max-def-len'
          (mapc (lambda (hint)
                  (setf (cdr hint)
                        (let ((binding (cdr hint)))
                          (if binding
                              (ggui--fix-len-visual binding max-def-len 'right)
                            (make-string max-def-len ?\s)))))
                column)))
    ;; flat matric into a list with padding
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
  ;; TODO use a clearer pattern matching syntax
  ;; to match elements in keymap
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
                                 (setq help (let ((help (condition-case nil
                                                            (nth 1 def)
                                                          (wrong-type-argument nil))))
                                              (if (stringp help) help nil)))
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

;;;; Biggiebuffer
;;
;; each app has a single biggiebuffer that shares history

;;;;; Variable, command, minor mode

(defvar ggui-biggie-mode-line-format nil
  "Mode line format of biggie buffer.")

(defvar-local ggui-biggie-history nil
  "A list of user input history of biggiebuffer.")

(defvar-local ggui-biggie-finish-fn nil
  "The finish function used in `ggui-biggie-finish'.")

(defvar-local ggui-biggie-abort-fn nil
  "The abort function used in `ggui-biggie-abort'.")

(defvar-local ggui-biggie-update-fn nil
  "The update function used in `ggui-biggie-update'.")

(defvar ggui-biggie-pop-fn
  (lambda (_) (let ((window (display-buffer-in-side-window
                             (ggui--biggiebuffer (ggui-this-app))
                             '((side . bottom) (slot . -1))))) ; hint is 1
                (or window ;; TODO look into it
                    (pop-to-buffer (ggui--biggiebuffer (ggui-this-app))))))
  "Functions used to display biggiebuffer.

Takes one argument, the current page, and returns the window.

This function is only used when the current `ggui-page'
doesn't implement its specific pop up function (`ggui--pop-biggie').")

(defvar ggui--biggie-map (ggui-define-map "GGUI biggiebuffer\n\n"
                                          "C-c C-c" (ggui-fn-binding #'ggui-biggie-finish "Finish")
                                          "C-c C-k" (ggui-fn-binding #'ggui-biggie-abort "Abort"))
  "`ggui--map' for biggiebuffer.")

(define-minor-mode ggui-biggie-update-mode
  "In this mode biggiebuffer updates after every command."
  :lighter ""
  (if ggui-biggie-update-mode
      (add-hook 'ggui--after-change-functions #'ggui-biggie-update t t)
    (remove-hook 'ggui--after-change-functions #'ggui-biggie-update t)))

(defun ggui-biggie-finish ()
  "Finish with current content."
  (interactive)
  (let ((ggui-biggie-window (selected-window)))
    (ggui-biggie-update-mode -1)
    (add-to-list 'ggui-biggie-history (buffer-string))
    (if ggui-biggie-finish-fn
        (funcall ggui-biggie-finish-fn (buffer-string))
      (ggui-log :warn "No `ggui-biggie-finish-fn' specified."))
    (delete-window ggui-biggie-window)))

(defun ggui-biggie-abort ()
  "Abort with current content."
  (interactive)
  (let ((ggui-biggie-window (selected-window)))
    (ggui-biggie-update-mode -1)
    (if ggui-biggie-abort-fn
        (funcall ggui-biggie-abort-fn (buffer-string))
      (ggui-log :warn "No `ggui-biggie-abort-fn' specified."))
    (delete-window ggui-biggie-window)))

(defun ggui-biggie-update (_ _1 _2)
  "Update with current content."
  (if ggui-biggie-update-fn
      (funcall ggui-biggie-update-fn (buffer-string))
    (ggui-log :warn "No `ggui-biggie-update-fn' specified.")))

;;;;; Internal API

(defun ggui--make-biggie-buffer (name)
  "Create a new biggiebuffer bas on NAME."
  (let ((buf (generate-new-buffer (format " *biggie-%s*"name))))
    (with-current-buffer buf
      (setq mode-line-format ggui-biggie-mode-line-format))
    buf))

(defun ggui-invoke-biggie (finish-callback abort-callback &optional update-callback)
  "Invoke biggiebuffer with FINISH-CALLBACK and ABORT-CALLBACK.

FINISH-CALLBACK is called with biggiebuffer's content when the user finish,

ABORT-CALLBACK is called with biggiebuffer's content when the user abort.
If non-nil, UPDATE-CALLBACK will be called with biggiebuffer's content
whenever biggiebuffer's content has changed.

This function should be called inside other functions for a user input."
  (select-window (ggui--pop-biggie (ggui-this-page)))
  (switch-to-buffer (ggui--biggiebuffer (ggui-this-app)))
  (erase-buffer)
  (setq ggui-biggie-finish-fn (lambda (str)
                                (funcall finish-callback str))
        ggui-biggie-abort-fn abort-callback
        ggui-biggie-update-fn update-callback)
  (ggui-use-map-default ggui--biggie-map)
  (if update-callback
      (ggui-biggie-update-mode)
    (ggui-biggie-update-mode -1)))

(cl-defgeneric ggui--pop-biggie ((page ggui-page))
  "Pop a buggiebuffer. Specific location of the window and size
depends on each PAGE. By default use a side window.
Return the window used for displaying biggiebuffer,
nil if no suitable window can be found."
  (ignore page)
  (funcall ggui-biggie-pop-fn (ggui-this-page)))


;;;; Node

;;;;; Class
(ggui-defclass ggui-node ()
  ((parent
    :type (or null ggui-node)
    :initform nil
    :documentation "The parent of this node.")
   (children
    :type list
    :initform nil
    :documentation "A list of children."))
  "A node in a tree.")

(cl-defmethod initialize-instance :after ((node ggui-node) &rest _)
  (dolist (child (ggui--children node))
    (setf (ggui--parent child) node)))

;;;;; Methods

(cl-defgeneric ggui-put-under-at ((child ggui-node) (parent ggui-node) n)
  "Put CHILD under PARENT at position n."
  (setf (ggui--parent child) parent)
  (ggui-insert-at-n child (ggui--children parent) n))

(cl-defgeneric ggui-put-under-end ((child ggui-node) (parent ggui-node))
  "Put CHILD under PARENT at the end of its children list."
  (ggui-put-under-at child parent (length (ggui--children parent))))

(cl-defgeneric ggui-put-under-begin ((child ggui-node) (parent ggui-node))
  "Put CHILD under PARENT at the beginning of its children list."
  (ggui-put-under-at child parent 0))

(cl-defgeneric ggui-put-under-after ((node-after ggui-node) (node ggui-node))
  "Put NODE-AFTER after NODE under NODE's parent.
If NODE-BEFORE doesn't have parent, error."
  (let* ((parent (ggui--parent node))
         (index-of-node (ggui--find-index node (ggui--children parent))))
    (if index-of-node
        (ggui-put-under-at node-after parent (1+ index-of-node))
      (signal 'ggui-element-missing (list (format "You try to put NODE-AFTER %s after NODE %s,
but somehow NODE is not in its parent's `children' list, parent is %s"
                                                  node-after node parent))))))

(cl-defgeneric ggui-put-under-before ((node-before ggui-node) (node ggui-node))
  "Put  NODE-BEFORE before NODE under NODE's parent.
If NODE-BEFORE doesn't have parent, error."
  (let* ((parent (ggui--parent node))
         (index-of-node (ggui--find-index node parent)))
    (if index-of-node
        (ggui-put-under-at node-before parent index-of-node)
      (signal 'ggui-element-missing (list (format "You try to put NODE-BEFORE %s before NODE %s,
but somehow NODE is not in its parent's `children' list, parent is %s"
                                                  node-before node parent))))))

(cl-defgeneric ggui-remove-from ((child ggui-node) (parent ggui-node))
  "Remove CHILD from PARENT."
  (setf (ggui--parent child) nil)
  ;; TODO use ggui-remove
  (setf (ggui--children parent) (remove child (ggui--children parent))))

(cl-defmethod ggui--last-child-p ((node ggui-node))
  "Return t if NODE is the last child in its parent's children list.
If NODE doesn't have parent, return t.
Return nil otherwise."
  (when-let ((parent (ggui--parent node)))
    (let ((seq (ggui--children parent)))
      (eq node (seq-elt seq (1- (length seq)))))))

;;;; lazy node

(ggui-defclass ggui-lazy-node (ggui-node)
  ()
  "lazy-node is a type of node with on-the-fly calculated children."
  :abstract t)

(cl-defmethod initialize-instance :after ((node ggui-lazy-node) &rest _)
  (setf (slot-value node 'raw-children) (slot-value node 'children))
  (slot-makeunbound node 'children))

(cl-defmethod ggui--children ((node ggui-lazy-node))
  (if (or (not (slot-boundp node 'children))
          (ggui--should-update node))
      ;; children never set up (first time)
      ;; or should update
      (setf (slot-value node 'children) (ggui--generate-children node))
    (slot-value node 'children)))

(cl-defgeneric ggui--generate-children ((node ggui-lazy-node))
  "Generate and return a children list for NODE.")

(cl-defgeneric ggui--should-update ((node ggui-lazy-node))
  "Return t if NODE should update its children.")

;;;; Mix and match text padding
;;
;; Notes on accessing slots:
;; always use the accessor `ggui--<slot>',
;; never use `slot-value'!
;; because some classes (like this one)
;; precess the slot specially before
;; returning it.
;;
;; In the case of `ggui-indent-view',
;; it adds indent before return.

(cl-defgeneric ggui--pad (text (view ggui-view) style)
  "Return the padded TEXT.
VIEW is used for additional information for padding.
STYLE is the style of prefix, it could be 'space, 'node, 'cell.")

;;;; node-view

(ggui-defclass ggui-node-view (ggui-view ggui-node ggui-default-toggle)
  ((indent-level
    :initform 0
    :type integer
    :documentation "The level of indent.")
   (lazy
    :read-only t
    :type boolean
    :initform nil
    :documentation "A node view is lazy if its children doesn't follow it when it is displayed.
Instead, they are shown when the node expands.")
   (state-count
    :initform 2
    :read-only t))
  "A node that is also a view.
A `ggui-node-view''s children have to be all `ggui-node-view's.")

(cl-defmethod ggui--text ((node ggui-node-view))
  (ggui--pad (slot-value node 'text) node 'node))

;;;;; Method of indent-view

(cl-defmethod ggui--prefix-for-children ((node ggui-node-view))
  "Return the indent prefix of the NODE."
  (if-let ((parent (ggui--parent node)))
      (concat (ggui--prefix-for-children parent)
              (if (ggui--last-child-p node)
                  "  "
                "│ "))
    ""))

(cl-defmethod ggui--pad (text (view ggui-node-view) (style (eql space)))
  (ignore style)
  (concat (make-string (* 2 (ggui--indent-level view)) ?\s)
          text))

(cl-defmethod ggui--pad (text (node ggui-node-view) (style (eql node)))
  (ignore style)
  (concat (if (eq 0 (ggui--indent-level node)) ; no parent
              ""
            (concat (ggui--prefix-for-children (ggui--parent node))
                    (if (ggui--last-child-p node)
                        "└" "├")
                    "─"))
          text))

;;;;; Side effect of node and indent-view and view

;;;;;; node

(cl-defmethod ggui-put-under-at :after ((child ggui-node-view) (parent ggui-node-view) n)
  "Put CHILD under PARENT at position n."
  ;; this doesn't modify the children list,
  ;; but the side does execute, which is what we want
  (ggui-insert-at child (ggui--children parent) n)
  ;; change indent level
  (ggui--sync-indent child))

;;;;;; view & node

(cl-defmethod ggui-put-after ((node ggui-node-view) (node-b ggui-node-view))
  (let ((children (ggui--children node-b)))
    (if (and (not (eq (ggui--parent node) node-b))
             children)
        ;; if node-b has children,
        ;; put node after its children
        (if (ggui--lazy node-b)
            ;; if node-b is lazy, it can be excused if children are not displayed
            ;; this is save because laziness is read-only
            (condition-case nil
                (ggui-put-after node children)
              (ggui-view-not-present (cl-call-next-method node node-b)))
          (ggui-put-after node children))
      ;; otherwise just put node after node-b as usual
      (cl-call-next-method node node-b)))
  node)

(cl-defmethod ggui-expand-children ((node ggui-node-view) &optional depth)
  "Expand and show NODE's children.
DEPTH is maximum levels of children to display, nil means 1.
Set DEPTH to 'infinity if you want to expand all possible levels.
Return NODE."
  (when-let ((children (ggui--children node)))
    (if (ggui--presentp children)
        (ggui-show children)
      (ggui-put-after children node))
    (unless (or (not depth) ; depth = nil
                (eq depth 1))
      (dolist (child children)
        (ggui-expand-children child (1- depth))))
    node))

(cl-defmethod ggui-collapse-children ((node ggui-node-view))
  "Collapse all children of NODE.
Return NODE."
  (when-let ((children (ggui--children node)))
    (mapc #'(lambda (child)
              (ggui-collapse-children child)
              (ggui-hide child))
          children))
  node)

(cl-defgeneric ggui--children-follow-node ((node ggui-node-view))
  "Make children follow node when node is displayed."
  (unless (ggui--lazy node)
    (ggui-expand-children node)))

(cl-defmethod ggui-put-after :after ((node ggui-node-view) _)
  ;; if you put A after B, A's children should follow A
  (ggui--children-follow-node node))

(cl-defmethod ggui-put-before :after ((node ggui-node-view) _)
  ;; if you put A after B, A's children should follow A
  (ggui--children-follow-node node))

;;;;;; indent (& node)

(cl-defmethod initialize-instance :after ((node ggui-node-view) &rest _)
  (ggui--sync-indent node))

(cl-defmethod ggui--sync-indent ((node ggui-node) &optional parent-level)
  "Sync the `indent-level' of node with its parent and children.
PARENT-LEVEL is used internally."
  ;; if parent-level is specified, save the effort to retrieve level from
  ;; parent. Pass children my level to save them the effort
  (setf (ggui--indent-level node)
        (if (ggui--parent node)
            (1+ (or parent-level (ggui--indent-level (ggui--parent node))))
          0))
  (when-let ((children (ggui--children node)))
    (dolist (child children)
      (ggui--sync-indent child (ggui--indent-level node)))))

(cl-defmethod ggui-remove-from :after ((child ggui-node-view) (_ ggui-node-view))
  "Set indent-level of CHILD to 0 when removed."
  (setf (ggui--indent-level child) 0))

;;;;; Toggle

(cl-defmethod ggui-toggle-to ((node ggui-node-view) (n (eql 0)))
  "Collapse NODE's children."
  (ignore n)
  (ggui-collapse-children node))

(cl-defmethod ggui-toggle-to ((node ggui-node-view) (n (eql 1)))
  "Collapse NODE's children."
  (ignore n)
  (ggui-expand-children node))


;;;; table
;;
;; like tabulated-mode but base on ggui-views

;;;;; Userland

(defface ggui-table-header-face '((t (:inherit header-line)))
  "Face of ggui table headers."
  :group 'ggui)

(defvar ggui--local-table nil
  "The local `ggui-table' displayed.

`ggui-table' has two slots, TABLE and FORMAT.

TABLE is a list of rows. Each row is also a
list, each element of a row is a `ggui-table-cell'.

FORMAT specifies the header format of `ggui--local-table'.
Similar to `tabulated-list-format' but is a list.
Each element is (NAME WIDTH ALIGN-FN SORT-FN).

NAME is a string displayed as header for each column.

WIDTH is the reserved width for the column.
Note that the WIDTH is the visual length.
That basically means CJK chars are 2 unit wide.
More information in `ggui--visual-length'
and `string-width'.

(optional) ALIGN-FN is a function that takes two arguments:
TEXT and WIDTH, and pad/trim and align TEXT to WIDTH.
GGUI comes with some align functions:
`ggui-table-align-left', `ggui-table-align-right'.
Value of nil means use `ggui-table-align-left.'

(optional) SORT-FN is a function that takes to arguments
TEXT1 and TEXT2 and returns t if TEXT1 should
be placed before TEXT2 and vise versa.
Value of nil means no sorting mechanism is provided for this column.")

(cl-defstruct ggui-table
  "More info in `ggui--local-table'."
  table ; 2D list
  format
  )

(defun ggui-use-table (gtable &optional buffer)
  "Put `ggui-table' GTABLE into BUFFER or current buffer. Return nil.
Be aware that BUFFER (or current buffer) is erased before inserting table."
  (let ((buffer (or buffer (current-buffer))))
    (ggui--setup-buffer buffer t)
    (with-current-buffer buffer
      (ggui-put-after (ggui-table-table gtable)
                      ggui-top-view)
      (setq ggui--local-table gtable)
      (setq header-line-format
            (ggui--make-table-header (ggui-table-format gtable))))))

;; TODO ggui-insert-column
;; when its needed

;;;;; Backstange


(defun ggui-table-align-left (text width)
  "Align TEXT to left, trim to WIDTH if needed."
  (ggui--fix-len-visual text width 'right))

(defun ggui-table-align-right (text width)
  "Align TEXT to left, trim to WIDTH if needed."
  (ggui--fix-len-visual text width 'left))

(defun ggui--add-cell-segment (text)
  "Return TEXT with segment added to it."
  (concat text " "))

(defun ggui--make-table-header (format)
  "Return a string to display in header line.
for FORMAT see `ggui--local-table-format'."
  ;; TODO clicking commands?
  (apply #'concat
         (mapcar (lambda (column)
                   (ggui--add-cell-segment
                    (let ((name (nth 0 column))
                          (width (nth 1 column)))
                      (propertize
                       (ggui--fix-len-visual name width 'right)
                       'face 'ggui-table-header-face))))
                 format)))

;;;; table-cell

(ggui-defclass ggui-table-cell (ggui-view)
  ((column
    :type integer
    :documention "0-based column number of the cell."))
  "A cell of `ggui--local-table'.")

(cl-defmethod ggui--pad (text (cell ggui-table-cell) (style (eql cell)))
  (ignore style)
  ;; Make TEXT into fixed length and add segment at end
  (concat (if ggui--local-table
              (let* ((header-format (ggui-table-format ggui--local-table))
                     (format-for-this-column ; (TEXT WIDTH ALIGN-FN SORT-FN)
                      (nth (ggui--column cell) header-format))
                     (width (nth 1 format-for-this-column))
                     (align-fn (nth 2 format-for-this-column)))
                (or (funcall (or align-fn #'ggui-table-align-left)
                             text width)
                    ;; don't trust users
                    (progn
                      (ggui-log :error (format "ALIGN-FN of column %d should not return nil, but it did"
                                               (ggui--column cell)))
                      text)))
            ;; (ggui-log :warn "`ggui--local-table-format' is nil.")
            text)
          ;; segment
          " "))

(cl-defmethod ggui--text ((cell ggui-table-cell))
  (ggui--pad (slot-value cell 'text) cell 'cell))

(ggui-defclass ggui-new-line (ggui-view)
  ((text
    :initform "\n"
    :read-only t))
  "A `ggui-view' which the `text' slot is new line.")

(defun ggui-make-cell (cell-or-string &optional column)
  "Return a `ggui-table-cell' from CELL-OR-STRING.
COLUMN is column number, if nil, don't set it."
  (let ((cell (pcase cell-or-string
                ((pred stringp) (ggui-table-cell :text cell-or-string))
                ((pred (lambda (obj) (object-of-class-p obj 'ggui-table-cell))) cell-or-string)
                (_ (signal 'wrong-type-argument (list "CELL-OR-STRING should be either `ggui-table-cell' or string."))))))
    (when column
      (setf (ggui--column cell) column))
    cell))

(defun ggui-make-row (cell-lst &rest other-cell-lst)
  "Make a row out of CELLs.

CELL-LST is a list of CELLs.
Rest CELLs in OTHER-CELL-LST are individual CELLs.

CELLs can be string or `ggui-table-cell', strings are converted
to `ggui-table-cell' automatically."
  (let ((all-cell-lst (append cell-lst other-cell-lst)))
    (append (cl-loop for cell in all-cell-lst
                     for column-idx from 0 to (1- (length all-cell-lst))
                     collect (ggui-make-cell cell column-idx))
            (list (ggui-new-line)))))

(defun ggui-make-table (row-lst &rest other-row-lst)
  "Make a table out of ROWs.
ROW-LST is a list of rows,
rest ROWs in OTHER-ROW-LST are individual rows.

CELLs in a ROW can be string or `ggui-table-cell', strings are converted
to `ggui-table-cell' automatically."
  (mapcar #'ggui-make-row (append row-lst other-row-lst)))

;;;;; node table cell

(ggui-defclass ggui-cell-node (ggui-node-view ggui-table-cell)
  ()
  "A `ggui-table-cell' that serves both as a table cell and a tree node.")

(cl-defmethod ggui--text ((node ggui-cell-node))
  (ggui--pad (ggui--pad (slot-value node 'text) node 'node)
             node 'cell))

;; TODO tabs

;;;; Provide

(provide 'ggui)

;;; ggui ends here
