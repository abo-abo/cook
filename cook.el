;;; cook.el --- An Elisp wrapper for Python pycook -*- lexical-binding: t -*-

;; Copyright (C) 2017 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/cook
;; Version: 0.1.0
;; Package-Requires: ((ivy "0.9.1"))
;; Keywords: python, makefile

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

(require 'ivy)
(require 'compile)

(defun cook-current-cookbook ()
  "Find Cookbook.py in the current project."
  (let ((as-file (locate-dominating-file default-directory "Cookbook.py"))
        (as-dir (locate-dominating-file default-directory "cook")))
    (cond ((null as-file)
           (if as-dir
               (expand-file-name "cook/Cookbook.py" as-dir)
             (error "No `Cookbook.py' or `cook/Cookbook.py' found")))
          ((null as-dir)
           (expand-file-name "Cookbook.py" as-file))
          ((> (length as-file) (length as-dir))
           (expand-file-name "Cookbook.py" as-file))
          (t
           (expand-file-name "Cookbook.py" as-dir)))))

(defun cook-slurp (f)
  "Read contents of file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun cook-recompile ()
  "Wrap around `recompile'.
This command expects to be bound to \"g\" in `comint-mode'."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (self-insert-command 1)
    (let ((default-directory (or compilation-directory default-directory)))
      (compilation-start
       (car compilation-arguments)
       t
       (lambda (_) (buffer-name))))
    (cook-comint-mode)))

(defvar cook-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'cook-recompile)
    map)
  "Keymap for `cook-comint-mode'.")

(define-minor-mode cook-comint-mode
  "Minor mode for `comint-mode' buffers produced by `compile'."
  :keymap cook-comint-mode-map)

(defvar cook-history nil
  "History for `cook'.")

;;;###autoload
(defun cook (arg)
  "Locate Cookbook.py in the current project and run one recipe.

When ARG is non-nil, open Cookbook.py instead."
  (interactive "P")
  (if arg
      (find-file (cook-current-cookbook))
    (let* ((book (cook-current-cookbook))
           (default-directory (file-name-directory book))
           (contents (cook-slurp book))
           (recipes
            (delq nil
                  (mapcar (lambda (s)
                            (when (string-match "\\`def \\(.*\\)(recipe):\\'" s)
                              (match-string 1 s)))
                          (split-string contents "\n" t))))
           (recipe (ivy-read "recipe: " recipes
                             :preselect (car cook-history)
                             :history 'cook-history))
           (cmd (shell-command-to-string
                 (format "python -c 'import Cookbook as c; print(\"\\n\".join(c.%s(42)))'"
                         recipe))))
      (setf (car cook-history) recipe)
      (if (require 'mash nil t)
          (progn
            (when (file-remote-p book)
              (setq book
                    (tramp-file-name-localname
                     (tramp-dissect-file-name book))))
            (setq mash-new-compilation-cmd (format "%s %s" book recipe))
            (mash-make-shell recipe 'mash-new-compilation))
        (with-current-buffer (compile cmd t)
          (cook-comint-mode))))))

(provide 'cook)
