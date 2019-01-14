(require 'ggud)

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
