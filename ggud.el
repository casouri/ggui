;;; ggud.el --- Grand GGUI Unified Debugger      -*- lexical-binding: t; -*-

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

;;;; Etc

(defun ggud-goto (line column &optional buffer file)
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

(defun ggud--translate-pos (line column buffer)
  "Translate LINE:COLUMN in BUFFER to POS in BUFFER.
Return (POS BUFFER)."
  (cons (progn (ggui-goto line column buffer) (point))
        buffer))

(provide 'ggud)

;;; ggud.el ends here
