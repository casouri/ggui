
;;; ggui-test.el --- Tests for ggui.el      -*- lexical-binding: t; -*-

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; This is tests for ggui.el

;;; Code:
;;

(require 'ert)
(require 'ggui)

;;; Test helper
(defun ggui-test-get-unique-buffer ()
  "Return a buffer with a unique name."
  (generate-new-buffer "ggui-test"))

(defmacro ggui-test-with-buffer (buffer-symbol-list &rest body)
  "Eval BODY with buffers, then cleans up buffers.

It works kind of `gensym':
BUFFER-SYMBOL-LIST is a list of buffer symbols.
\(ggui-test-with-buffer (buf1 buf2) (body)) gives you
two unique buffer buf1 and buf2 and evaluates (body).

It also wraps everything in `save-excursion' for convenience."
  (declare (indent 1))
  `(save-excursion
     (let ,(mapcar (lambda (sym) (list sym '(ggui-test-get-unique-buffer)))
                   buffer-symbol-list)
       (unwind-protect
           (progn ,@body)
         (mapc #'kill-buffer
               (list ,@buffer-symbol-list))))))


;;; Helper
;; TODO
(ert-deftest ggui-defclass ()
  (should t))

;;; Test
(ert-deftest ggui-goto ()
  (ggui-test-with-buffer (buffer buffer1)
    (let ()
      ;; 0:0
      (ggui-goto 0 0 buffer)
      (should (eq (point) 1))
      ;; random pos
      (with-current-buffer buffer1 (insert "1234\n678\n01234\n"))
      (ggui-goto 2 3 buffer1)
      (should (eq (point) 13))
      ;; TODO go to file
      ;; out of range
      (should-error (ggui-goto 999999 99999 buffer))
      ;; not enough arg
      (should-error (ggui-goto 0 0))
      ;; buffer missing
      (should-error (ggui-goto 0 0 (generate-new-buffer-name "?????????????")))
      ;; file missing
      (should-error (ggui-goto 0 0 nil "????"))
      nil)))

(ert-deftest ggui-view ()
  (ggui-test-with-buffer (buf1 buf2)
    (let ((view1 (ggui-view-new "hi"))
          (view2 (ggui-view-new "yo")))
      (with-current-buffer buf1
        ;; setup
        (ggui--setup-buffer buf1)
        (should (equal "T\n\nB" (buffer-string)))
        ;; overlay property
        (should (eq (plist-get (overlay-properties (ggui--overlay view1)) 'ggui-view)
                    view1))
        (ggui--overlay-put view1 'woomy 'good 'veemo 'nice)
        (should (eq 'good (plist-get (overlay-properties (ggui--overlay view1)) 'woomy)))
        (should (eq 'nice (plist-get (overlay-properties (ggui--overlay view1)) 'veemo)))
        ;; put
        (ggui-put-after view1 ggui--top-view)
        (should (equal "T\n\nhi\n\nB" (buffer-string)))
        (ggui-put-before view2 ggui--bottom-view)
        (should (equal "T\n\nhi\n\nyo\n\nB" (buffer-string)))
        ;; remove
        (ggui--remove-display view1)
        (should (equal "T\n\nyo\n\nB" (buffer-string)))
        ;; auto update
        (setf (ggui--text view2) "yooooo")
        (should (equal "T\n\nyooooo\n\nB" (buffer-string)))
        ;; error
        (should-error (ggui-put-before "!" ggui--top-view))
        (should-error (ggui-put-after "!" ggui--bottom-view))))))

(ert-deftest ggui-seq ()
  (ggui-test-with-buffer (buf1)
    (let* ((view1 (ggui-view-new "woomy"))
           (view2 (ggui-view-new "veemo"))
           (view3 (ggui-view-new "fresh"))
           (seq (list view1 view2 view3))
           (view99 (ggui-view-new "loove"))
           (view98 (ggui-view-new "mary"))
           (ssq (list seq view98)))
      (with-current-buffer buf1
        (should (equal (list view1 view2) (list view1 view2)))
        ;; setup
        (ggui--setup-buffer buf1)
        ;; put after
        (ggui-put-after seq ggui--top-view)
        (should (equal "T\n\nwoomy\n\nveemo\n\nfresh\n\nB" (buffer-string)))
        ;; put at 3rd place
        (ggui-put-at view99 seq 2)
        (print seq)
        (should (equal "T\n\nwoomy\n\nveemo\n\nloove\n\nfresh\n\nB" (buffer-string)))
        (should (equal seq (list view1 view2 view99 view3)))
        ;; delete veemo
        (ggui-delete view2 seq)
        (should (equal "T\n\nwoomy\n\nloove\n\nfresh\n\nB" (buffer-string)))
        (should (equal seq (list view1 view99 view3)))
        ;; put view to seq
        (ggui-put-after view2 seq)
        (should (equal "T\n\nwoomy\n\nloove\n\nfresh\n\nveemo\n\nB" (buffer-string)))
        ;; remove seq's display
        (ggui--remove-display seq)
        (should (equal "T\n\nveemo\n\nB" (buffer-string)))
        ;; list of list
        (ggui-put-after ssq view2)
        (should (equal "T\n\nveemo\n\nwoomy\n\nloove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; car of ssq is seq, put at 2nd position
        (ggui-put-at view2 (car ssq) 1)
        (should (equal "T\n\nwoomy\n\nveemo\n\nloove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; edit a element from a list
        (setf (ggui--text (nth 2 seq)) "love")
        (should (equal "T\n\nwoomy\n\nveemo\n\nlove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; put seq at somewhere in ssq
        (ggui-put-at seq ssq -1)
        (should (equal "T\n\nmary\n\nwoomy\n\nveemo\n\nlove\n\nfresh\n\nB" (buffer-string)))
        ))))

;;; ggui-test ends here
