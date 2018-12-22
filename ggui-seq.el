(cl-defgeneric ggui-delete (elt seq)
  "Delete ELT from SEQ.")

(cl-defmethod ggui-delete (elt (seq list))
  (delete elt seq))

(cl-defgeneric ggui-append (seq1 seq2)
  "Append seq and seq2.")

(cl-defmethod ggui-append ((seq1 list) (seq2 list))
  (append seq1 seq2))

(cl-defgeneric ggui-insert-at (elt seq n)
  "Insert ELT into SEQ at Nth position.")

(defsubst ggui--regulate-index (n len)
  "Regulates positive/negative argument N for sequence of LEN."
  (if (>= n 0) n ; translate negative index n to positive
    ;; n=-1 -> nn=len, n=-2 -> nn=(len - 1)
    (+ len n 1)))

(cl-defmethod ggui-insert-at (obj (seq list) n)
  "Simply insert OBJ into SEQ at N.
If N is larger than length of SEQ, signal `args-out-of-range'.
If N is negative, insert at last Nth position.
Return the modified seq."
  (let* ((len (length seq))
         (nn (ggui--regulate-index n len)))
    ;; check
    (when (> (abs n) len) (signal 'args-out-of-range (list "SEQ is:" seq "N is:" n)))
    ;; insert
    (setf (nthcdr nn seq) (cons obj (nthcdr nn seq))))
  seq)

;; We don't need to remove view before adding it because
;; it is done automatically for us in `ggui-put-after/before'

(cl-defmethod ggui-put-at (view (seq sequence) n)
  "Put VIEW at N in SEQ (a list of `ggui-view's).
VIEW is not added again if it's already in seq,
instead, it is moved to N.
If N is larger than length of SEQ, signal `args-out-of-range'.
If N is negative, put at last N position.
E.g., -1 is the end of SEQ.

VIEW can be either `ggui-view' or a list of them,
or a list of list of them, etc.

Unlike `ggui-put-before/after',
this function changes SEQ because it adds VIEW into it.
Return SEQ."
  (unless (or (cl-typep view 'ggui-view)
              (cl-typep view 'list))
    (signal 'invalid-argument (list "Only ggui-view or list accepted for VIEW, you gave:" view)))
  ;; remove if present
  (when (seq-find (lambda (elt) (equal elt view)) seq nil)
    ;; same as above, when adding, view's display is removed
    (ggui-delete view seq))
  (let* ((len (seq-length seq))
         (nn (ggui--regulate-index n len)))
    (cond ((= nn len) (ggui-put-after view (seq-elt seq (1- len))))
          ((< nn len) (ggui-put-before view (seq-elt seq nn)))
          (t (signal 'args-out-of-range (list "SEQ is:" seq "N is:" n))))
    (ggui-insert-at view seq nn))
  seq)

;; seq to view
(cl-defmethod ggui-put-before ((seq sequence) (view ggui-view))
  "Put every view in SEQ before VIEW, in normal order.
E.g., SEQ: (1 2 3 4) VIEW: 5 -> 1 2 3 4 5.
This function is recursive.
Return nil."
  (let ((last-right view)
        index
        (len (seq-length seq))
        this)
    (seq index (1- len))
    (while (>= index 0)
      (setq this (seq-elt seq index))
      (cl-decf index)
      (ggui-put-before this last-right)
      (setq last-right this)))
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

(cl-defmethod ggui-append ((seq1 sequence) (seq2 sequence))
  "Append two list of `ggui-view's and return the merged list."
  (ggui-put-after (seq-elt seq1 (1- (seq-length seq1))) seq2)
  (ggui-append seq1 seq2))

(cl-defmethod ggui-delete :after (view (seq sequence))
  "Remove VIEW from SEQ.
VIEW can be either a `ggui-view' or a list of them"
  (unless (or (cl-typep view 'ggui-view)
              (cl-typep view 'list))
    (signal 'invalid-argument (list "Only ggui-view or list accepted for VIEW, you gave:" view)))
  (ggui--remove-display view))

(cl-defmethod ggui-put-under-at ((view ggui-view) (node cons) n)
  "Put VIEW under NODE at position N.
N can be positive or negative."
  (ggui-put-at view (cdr node) n))

;; TOTEST
;; TODO; is this useful?
(cl-defmethod ggui-apply-tree (func (node cons) &rest args)
  "Apply FUNC with NODE and ARGS recursively.
Depth first, left first."
  (let ((car (car node))
        (cdr (cdr node)))
    (apply #'ggui-apply-tree func car args)
    (apply func car args)
    (when cdr
      (apply #'ggui-apply-tree func cdr args)
      (apply func cdr args))))

(cl-defmethod ggui--remove-display ((seq sequence))
  "Remove display of SEQ.
This function is recursive."
  (seq-map #'ggui--remove-display seq))
