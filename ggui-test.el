
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
      ;; out of range
      (should-error (ggui-goto 999999 99999 buffer))
      ;; not enough arg
      (should-error (ggui-goto 0 0))
      ;; buffer missing
      (should-error (ggui-goto 0 0 (generate-new-buffer-name "?????????????")))
      ;; file missing
      (should-error (ggui-goto 0 0 nil "????"))
      nil)))

;;; ggui-test ends here
