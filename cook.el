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

(defun cook-reselect (&optional arg)
  (interactive "P")
  (if (get-buffer-process (current-buffer))
      (self-insert-command 1)
    (cook arg)))

(defun cook-do-bury-buffer (b)
  (switch-to-buffer
   (cl-find-if
    (lambda (b)
      (and (buffer-live-p b)
           (not (eq (aref (buffer-name b) 0) ?\s))
           (with-current-buffer b
             (not cook-comint-mode))))
    (cdr (buffer-list)))))

(defun cook-bury-buffer ()
  "Wrap around `bury-buffer'.
This command expects to be bound to \"q\" in `comint-mode'."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (self-insert-command 1)
    (cook-do-bury-buffer (current-buffer))))

(defvar cook-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'cook-reselect)
    (define-key map (kbd "g") 'cook-recompile)
    (define-key map (kbd "q") 'cook-bury-buffer)
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
    (when (buffer-file-name)
      (save-buffer))
    (let* ((book (cook-current-cookbook))
           (default-directory (if (string-match "\\`\\(.*/\\)cook/Cookbook.py" book)
                                  (match-string-no-properties 1 book)
                                (file-name-directory book)))
           (recipes (split-string (shell-command-to-string
                                   "cook --list") "\n" t))
           (recipe (ivy-read "recipe: " recipes
                             :preselect (car cook-history)
                             :history 'cook-history))
           (cmd (format "cook %s" recipe))
           buf)
      (setf (car cook-history) recipe)
      (if (require 'mash nil t)
          (progn
            (when (file-remote-p book)
              (setq book
                    (tramp-file-name-localname
                     (tramp-dissect-file-name book))))
            (setq buf (mash-make-shell
                       recipe 'mash-new-compilation cmd))
            (with-current-buffer buf
              (cook-comint-mode)))
        (with-current-buffer (compile cmd t)
          (cook-comint-mode))))))

(provide 'cook)
