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
;;

;;; Helper

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
                                ;; :accessor
                                ;; (intern (format "uikit-%s-of" slot-name))
                                :reader
                                (intern (format "gguir-%s" slot-name))
                                :writer
                                (intern (format "gguiw-%s" slot-name))))))
              slot-list)
     ,@rest))

(defmacro ggui-signal (signal-symbol str&args &rest data)
  "Signal SIGNAL-SYMBOL with helpful information.
Basically a `signal' but the first data is a formatted string.

For consistency with `signal', you still need to quote SIGNAL-SYMBO,
event though this is a macro. You don't need to quote STR&ARGS.

STR&ARGS is a list like (str &rest args) that can be passed into `format-message'.
It works the same as `error'.

You can pass normal DATA as if to `signal'."
  `(signal ,signal-symbol (list (apply #'format-message (list ,@str&args))
                                ,@data)))

;; FIXME I set it to :info for develop reason
(defvar ggui-log-level :info
  "Log level of ggui, can be :error, :warning or :info.")

(defvar ggui--log-level-plist '(:error 3 :warning 2 :info 1)
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
                   (plist-get '(:warn "Warning" :error "Error" :info "Info") level)
                   str)
           args)))

;;; Error
;; TODO what error condition does this error belong to?
(define-error 'pos-out-of-range "Position is out of the range of the buffer (pos showed after colon)")

;;; Class
;;;; Position
(cl-defstruct ggui-buffer-pos
  "A position (line & column) in a buffer.
:line is 0-based, :column is 0-based, :buffer is a buffer object.
You can't use string for :buffer.

DO NOT use `make-ggui-buffer-pos', use `ggui-buffer-pos' instead."
  line column buffer)

;; just to be even with file-pos
(cl-defun ggui-buffer-pos (&key (line 0) (column 0) buffer path)
  "Return a `ggui-buffer-pos'.

* Arg
- :LINE
- :COLUMN
- :BUFFER :: buffer or string
- :PATH :: absolute path

You can only have one of buffer or path, if both are passed,
buffer is used.

* Error
- `file-missing'"
  (if buffer
      (make-ggui-buffer-pos :line line :column column :buffer buffer)
    (if (file-exists-p path)
        (make-ggui-buffer-pos :line line :column column :buffer )
      (ggui-signal 'file-missing ("Path: %s" path)))))

(cl-defmethod ggui-goto ((pos ggui-buffer-pos))
  "Go to POS.
* Args
- POS :: the position.

* Return
nil

* Error
- `pos-out-of-range' :: if POS is out of the range of the buffer it's in."
  (switch-to-buffer (ggui-buffer-pos-buffer pos))
  (goto-char 1)
  (condition-case-unless-debug nil
      (progn (forward-line (ggui-buffer-pos-line pos))
             (forward-char (ggui-buffer-pos-column pos))
             nil)
    (end-of-buffer (signal 'pos-out-of-range (list "pos is:" pos)))))


(provide 'ggui)

;;; ggui ends here
