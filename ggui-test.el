
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

(defmacro ggui-test-with-file (file-path-list &rest body)
  "Eval BODY with buffers, then cleans up files.

It works kind of `gensym':
FILE-PATH-LIST is an list of list of file symbol and paths, like in `let'.
\(ggui-test-with-file ((file1 \"/path1\") (file2 \"path2\")) (body)) gives you
two files file1 and file2 and evaluates (body).

It also wraps everything in `save-excursion' for convenience."
  (declare (indent 1))
  `(save-excursion
     (let ,(mapcar (lambda (lst) (list (car lst) (let ((buf (find-file-noselect (cadr lst))))
                                                   (with-current-buffer buf (write-file (cadr lst) nil)) buf)))
                   file-path-list)
       (unwind-protect
           (progn ,@body)
         (mapc (lambda (lst) (delete-file (cadr lst)))
               ',file-path-list)))))

;;; Helper
;; TODO
(ert-deftest ggui-defclass ()
  (should t))

;;; Class
;;;; Position
(ert-deftest ggui-goto ()
  (ggui-test-with-file ((file1 "/tmp/file1"))
    (ggui-test-with-buffer (buffer buffer1)
      (let ((pos0 (ggui-buffer-pos :line 0 :column 0 :buffer buffer))
            (pos1 (ggui-buffer-pos :line 2 :column 3 :buffer buffer1))
            (pos-err (ggui-buffer-pos :line 9999 :column 9999 :buffer buffer))
            (fpos (ggui-buffer-pos :line 0 :column 0 :path "/tmp/file1")))
        ;; pos0: test 0:0
        ;; pos1: test random place
        ;; pos-err: test out of range
        ;; fpos pos with file path
        ;;
        ;; pos0
        (ggui-goto pos0)
        (should (eq (point) 1))
        ;; pos1
        (with-current-buffer buffer1 (insert "1234\n678\n01234\n"))
        (ggui-goto pos1)
        (should (eq (point) 13))
        ;; pos-err
        (should-error (ggui-goto pos-err))
        ;; fpos
        ;; (ggui-goto fpos)
        ;; (should (eq (point) 1))
        ;; invalid path
        (should-error (ggui-buffer-pos :line 0 :column 0 :path "???"))))))



;;; ggui-test ends here
