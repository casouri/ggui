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

;;;; Test view

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
        (ggui-put-after view1 ggui-top-view)
        (should (equal "T\n\nhi\n\nB" (buffer-string)))
        (ggui-put-before view2 ggui-bottom-view)
        (should (equal "T\n\nhi\n\nyo\n\nB" (buffer-string)))
        ;; remove
        (ggui--remove-display view1)
        (should (equal "T\n\nyo\n\nB" (buffer-string)))
        ;; auto update
        (setf (ggui--text view2) "yooooo")
        (should (equal "T\n\nyooooo\n\nB" (buffer-string)))
        ;; error
        (should-error (ggui-put-before "!" ggui-top-view))
        (should-error (ggui-put-after "!" ggui-bottom-view))))))

(ert-deftest ggui-seq ()
  (ggui-test-with-buffer (buf1)
    (let* ((view1 (ggui-view-new "woomy"))
           (view2 (ggui-view-new "veemo"))
           (view3 (ggui-view-new "fresh"))
           (seq (list view1 view2 view3))
           (view99 (ggui-view-new "loove"))
           (view98 (ggui-view-new "mary"))
           ssq)
      (with-current-buffer buf1
        ;; setup
        (ggui--setup-buffer buf1)
        ;; put after
        (ggui-put-after seq ggui-top-view)
        (should (equal "T\n\nwoomy\n\nveemo\n\nfresh\n\nB" (buffer-string)))
        ;; put at 3rd place
        (ggui-insert-at-n view99 seq 2)
        (should (equal "T\n\nwoomy\n\nveemo\n\nloove\n\nfresh\n\nB" (buffer-string)))
        (should (equal seq (list view1 view2 view99 view3)))
        ;; delete veemo
        (ggui-remove-n view2 seq)
        (should (equal "T\n\nwoomy\n\nloove\n\nfresh\n\nB" (buffer-string)))
        (should (equal seq (list view1 view99 view3)))
        ;; put view to seq
        (ggui-put-after view2 seq)
        (should (equal "T\n\nwoomy\n\nloove\n\nfresh\n\nveemo\n\nB" (buffer-string)))
        ;; remove seq's display
        (ggui--remove-display seq)
        (should (equal "T\n\nveemo\n\nB" (buffer-string)))
        ;; list of list
        (setq ssq (list seq view98))
        (ggui-put-after ssq view2)
        (should (equal "T\n\nveemo\n\nwoomy\n\nloove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; car of ssq is seq, put at 2nd position
        (ggui-insert-at-n view2 seq 1)
        (should (equal "T\n\nwoomy\n\nveemo\n\nloove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; edit a element from a list
        (should (equal (nth 2 seq) view99))
        (setf (ggui--text (nth 2 seq)) "love")
        (should (equal "T\n\nwoomy\n\nveemo\n\nlove\n\nfresh\n\nmary\n\nB" (buffer-string)))
        ;; put seq at somewhere in ssq
        (ggui-remove-n (car ssq) ssq)
        (ggui-insert-at-n seq ssq 1)
        (should (equal "T\n\nmary\n\nwoomy\n\nveemo\n\nlove\n\nfresh\n\nB" (buffer-string)))
        ))))

;;;; node

(ert-deftest ggui-node ()
  (let* ((c1 (ggui-node))
         (c2 (ggui-node))
         (co (ggui-node))
         (coo (ggui-node))
         (p1 (ggui-node :children (list c1 c2))))
    (should (eq (ggui--parent c1) (ggui--parent c2)))
    (should (eq (ggui--parent c1) p1))
    (should (equal (ggui--children p1) (list c1 c2)))
    (ggui-put-under-end co p1)
    (should (equal (ggui--children p1) (list c1 c2 co)))
    (ggui-remove-from co p1)
    (should (equal (ggui--children p1) (list c1 c2)))
    (ggui-put-under-begin co p1)
    (should (equal (ggui--children p1) (list co c1 c2)))
    (ggui-put-under-after coo c1)
    (should (equal (ggui--children p1) (list co c1 coo c2)))))

;;;; node-view

(ert-deftest ggui-node ()
  (ggui-test-with-buffer (buf)
    (let* ((n1 (ggui-node-view :raw-text "100\n"))
           (n21 (ggui-node-view :raw-text "210\n"))
           (n22 (ggui-node-view :raw-text "220\n"))
           (n2 (ggui-node-view :raw-text "200\n" :children (list n21 n22)))
           (n3 (ggui-node-view :raw-text "300\n"))
           (ntop (ggui-node-view :raw-text "--------------------\n" :children (list n1 n2 n3))))
      (ggui--setup-buffer buf)
      (with-current-buffer buf
        (ggui-put-after ntop ggui-top-view)
        (should (equal (buffer-string) "T

--------------------


├───100


├───200


│   ├───210


│   └───220


└───300


B"))))))

;;;; test app

(ggui-defclass my-simple-app (ggui-app) ((name :initform "my simple app")))
(ggui-defclass my-simple-page1 (ggui-page) ())
(ggui-defclass my-simple-page2 (ggui-page) ())

(cl-defmethod initialize-instance :after ((app my-simple-app) &rest _)
  (setf (ggui--page-alist app) (list (cons 'page1 (my-simple-page1 :app app))
                                     (cons 'page2 (my-simple-page2 :app app)))))

(cl-defmethod initialize-instance :after ((page my-simple-page1) &rest _)
  (let ((buffer (get-buffer-create "HOME of page1")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "HOME, sweet home."))
    (setf (ggui--buffer-alist page) (list (cons 'main-buffer buffer)))))

(cl-defmethod initialize-instance :after ((page my-simple-page2) &rest _)
  (let ((buffer (get-buffer-create "COOL BUFFER2")))
    (with-current-buffer buffer
      (ggui--edit
       (erase-buffer)
       (insert "THIS IS A COOOOOL BUFFER, but belongs to page2.
You can't edit anything because it's in read only mode.")))
    (setf (ggui--buffer-alist page) (list (cons 'main-buffer buffer)))))

(defvar page1-map (ggui-define-map "This is map for page 1.\n\n"
                                   "C-c n" (ggui-fn-binding (lambda () (ggui-segue-to 'page2))
                                                            "Next")
                                   "C-c q" (ggui-fn-binding #'ggui-quit-app "Quit")))

(defvar page2-edit-map (ggui-define-map "Edit even it is read only.\n\n"
                                        "i" (ggui-fn-binding (lambda ()
                                                               (ggui-invoke-biggie
                                                                (let ((buffer (current-buffer)))
                                                                  (lambda (str) (with-current-buffer buffer
                                                                                  (ggui--edit
                                                                                   (goto-char (point-max))
                                                                                   (insert "\n" str)))))
                                                                (lambda (str) (message "Abort")))) "Insert" "Insert in the beginning")))
(defvar page2-map (ggui-define-map "This is map for page 2.\n\n"
                                   "C-c n" (ggui-fn-binding (lambda () (interactive) (ggui-segue-to 'page1))
                                                            "Next")
                                   "C-c q" (ggui-fn-binding #'ggui-quit-app "Quit")
                                   "C-c e" (ggui-map-binding page2-edit-map "Edit")))



(cl-defmethod ggui-segue (_ (to my-simple-page1))
  (message "PAGE1 is here!")
  ;; setup window layout
  (delete-other-windows)
  (switch-to-buffer (alist-get 'main-buffer (ggui--buffer-alist to)))
  (display-buffer-in-side-window (ggui--hint-buffer (ggui-this-app)) '((side . bottom)))
  (ggui-use-map-default page1-map))

(cl-defmethod ggui-segue :before ((_ my-simple-page1) __)
  (message "My simple page1 leaving!"))

(cl-defmethod ggui-segue (_ (to my-simple-page2))
  (message "PAGE2 is here!")
  (delete-other-windows)
  (switch-to-buffer (alist-get 'main-buffer (ggui--buffer-alist to)))
  (display-buffer-in-side-window (ggui--hint-buffer (ggui-this-app)) '((side . bottom)))
  (ggui-use-map-default page2-map)
  (read-only-mode))

(cl-defmethod ggui--open-app ((app my-simple-app))
  (let ((frame (make-frame)))
    (select-frame frame)
    (setf (ggui--frame app) frame)
    (set-frame-parameter frame 'ggui-app app)
    (setf (ggui--name app) (ggui-get-unique-name app))
    (add-to-list 'ggui-app-list app)
    (ggui-segue-to 'page1)))

(cl-defmethod ggui--quit-app ((app my-simple-app))
  (delete-frame (ggui--frame app)))

(cl-defmethod ggui--hide-app ((app my-simple-app))
  ())

(defvar my-simple-app nil)

(defun my-simple-app-start ()
  "Start my-simple-app."
  (interactive)
  (ggui--open-app (setq my-simple-app (my-simple-app))))

;;; ggui-test ends here
