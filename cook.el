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

;;; Commentary:
;;
;; The main entry point is M-x cook.
;; The recipes are defined in Cookbook.py.
;; See https://github.com/abo-abo/cook for examples.

;;; Code:

(require 'ivy)
(require 'compile)

(defgroup cook nil
  "An Elisp wrapper for Python pycook"
  :group 'tools
  :prefix "cook-")

(defun cook-current-cookbook ()
  "Find Cookbook.py in the current project."
  (let ((as-file (locate-dominating-file default-directory "Cookbook.py"))
        (as-dir (locate-dominating-file default-directory "cook")))
    (cond
      ((null as-file)
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
  (if (and (get-buffer-process (current-buffer))
           (equal (this-command-keys) "g"))
      (self-insert-command 1)
    (let ((dd default-directory)
          (cmd (string-trim (car compilation-arguments))))
      (erase-buffer)
      (let ((default-directory dd))
        (if (string-match "cook \\(:[^ ]+\\) \\(.*\\)\\'" cmd)
            (cook-book
             (match-string-no-properties 1 cmd)
             (match-string-no-properties 2 cmd))
          (cook nil (car (last (and (stringp cmd) (split-string cmd " "))))))))))

(defun cook-reselect ()
  "Select a different recipe from the current cookbook.
Or forward to `self-insert-command'."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (self-insert-command 1)
    (if (string-match "\\(:[^ ]+\\)" (buffer-name))
        (cook-book
         (match-string-no-properties 1 (buffer-name))
         nil)
      (cook))))

(defun cook-do-bury-buffer ()
  "Bury the current compilation buffer."
  (let ((visible-buffers (mapcar #'window-buffer (window-list))))
    (switch-to-buffer
     (cl-find-if
      (lambda (b)
        (and (buffer-live-p b)
             (not (eq (aref (buffer-name b) 0) ?\s))
             (not (member b visible-buffers))
             (with-current-buffer b
               (not (bound-and-true-p cook-comint-mode)))))
      (cdr (buffer-list))))))

(declare-function winner-undo "winner")

(defun cook-bury-buffer ()
  "Wrap around `bury-buffer'.
This command expects to be bound to \"q\" in `comint-mode'."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (self-insert-command 1)
    (cook-do-bury-buffer)))

(defvar cook-comint-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'cook-reselect)
    (define-key map (kbd "g") 'cook-recompile)
    (define-key map (kbd "q") 'cook-bury-buffer)
    map)
  "Keymap for `cook-comint-mode'.")

(define-minor-mode cook-comint-mode
  "Minor mode for `comint-mode' buffers produced by `compile'."
  :keymap cook-comint-mode-map
  :group 'cook
  (if cook-comint-mode
      (setq comint-scroll-to-bottom-on-output t)))

(defvar cook-history nil
  "History for `cook'.")

(defun cook--input-sentinel (process _msg)
  "Automatically restart PROCESS if sudo was needed."
  (when (eq (process-status process) 'exit)
    (with-current-buffer (process-buffer process)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^sudo: no tty present and no askpass program specified" nil t)
          (cook-recompile))))
    (advice-remove 'compilation-sentinel #'cook--input-sentinel)))

(declare-function tramp-file-name-localname "tramp")
(declare-function tramp-dissect-file-name "tramp")
(declare-function mash-make-shell "ext:mash")

(defun cook-select-buffer-window (buffer)
  "Select the window of BUFFER."
  (select-window
   (cl-find-if
    (lambda (w) (eq (window-buffer w) buffer))
    (window-list))))

(defvar cook-last-recipe ""
  "Store the last recipe.")

(defun cook-script (&rest args)
  "The script to run pycook on ARGS."
  (apply #'concat
         (if (file-remote-p default-directory)
             "python3 -m pycook"
           "cook")
         args))

;;;###autoload
(defun cook (&optional arg recipe)
  "Locate Cookbook.py in the current project and run RECIPE.

When ARG is non-nil, open Cookbook.py instead."
  (interactive "P")
  (unless (window-minibuffer-p)
    (cond ((equal arg '(16))
           (find-file (cook-current-cookbook)))
          (arg
           (ivy-read "book: "
                     (mapcar
                      #'file-name-base
                      (split-string
                       (shell-command-to-string
                        (cook-script " :"))
                       "\n" t))
                     :action (lambda (module)
                               (cook-book (concat ":" module) recipe))
                     :caller 'cook))
          (t
           (when (buffer-file-name)
             (save-buffer))
           (cook-book
            (cook-current-cookbook) recipe)))))

(defun cook-action-find-file (module)
  "Open MODULE in Emacs."
  (let ((fname
         (nth 1 (split-string
                 (shell-command-to-string
                  (cook-script " :" module))
                 "\n"))))
    (find-file fname)))

(ivy-set-actions
 'cook
 '(("f" cook-action-find-file "find-file")))

(defvar cook-last-cmd nil)

(defun cook-book (book recipe)
  "Select a RECIPE from BOOK and run it."
  (let* ((cook-cmd (if (string-match-p "\\`:" book)
                       (cook-script " " book)
                     (cook-script)))
         (default-directory (cond
                              ((string-match-p "\\`:" book)
                               default-directory)
                              ((string-match "\\`\\(.*/\\)cook/Cookbook.py" book)
                               (match-string-no-properties 1 book))
                              (t
                               (file-name-directory book))))
         (recipes
          (split-string (shell-command-to-string
                         (concat cook-cmd " --list")) "\n" t))
         (recipes-alist
          (mapcar (lambda (s) (cons (car (split-string s " :")) s)) recipes))
         (recipe (or recipe
                     (setq cook-last-recipe
                           (ivy-read "recipe: " recipes-alist
                                     :preselect cook-last-recipe
                                     :require-match t
                                     :history 'cook-history
                                     :caller 'cook-book))))
         (cmd (concat cook-cmd " " recipe))
         buf)
    (setq cook-last-cmd (list default-directory cmd))
    (advice-add 'compilation-sentinel :after #'cook--input-sentinel)
    (if (require 'mash nil t)
        (progn
          (when (file-remote-p book)
            (setq book
                  (tramp-file-name-localname
                   (tramp-dissect-file-name book))))
          (setq buf (mash-make-shell
                     (if (string-match-p "\\`:" book)
                         (concat book " " recipe)
                       recipe)
                     'mash-new-compilation cmd))
          (with-current-buffer buf
            (cook-comint-mode))
          (cook-select-buffer-window buf))
      (with-current-buffer (compile cmd t)
        (cook-comint-mode)))))

(provide 'cook)

;;; cook.el ends here
