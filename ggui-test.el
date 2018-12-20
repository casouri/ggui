
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

;;; Class
;;;; Position
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
        (ggui-put-after ggui--top-view view1)
        (should (equal "T\n\nhi\n\nB" (buffer-string)))
        (ggui-put-before ggui--bottom-view view2)
        (should (equal "T\n\nhi\n\nyo\n\nB" (buffer-string)))
        ;; remove
        (ggui--remove-display view1)
        (should (equal "T\n\nyo\n\nB" (buffer-string)))
        ;; auto update
        (setf (ggui--text view2) "yooooo")
        (should (equal "T\n\nyooooo\n\nB" (buffer-string)))
        ;; error
        (should-error (ggui-put-before ggui--top-view "?"))
        (should-error (ggui-put-after ggui--bottom-view "?"))))))

;;; ggui-test ends here
