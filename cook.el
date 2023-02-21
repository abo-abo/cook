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
  (let* ((as-file (locate-dominating-file default-directory "Cookbook.py"))
         (as-dir (locate-dominating-file default-directory "cook"))
         (book (cond
                ((null as-file)
                 (if as-dir
                     (expand-file-name "cook/Cookbook.py" as-dir)
                   (error "No `Cookbook.py' or `cook/Cookbook.py' found")))
                ((null as-dir)
                 (expand-file-name "Cookbook.py" as-file))
                ((> (length as-file) (length as-dir))
                 (expand-file-name "Cookbook.py" as-file))
                (t
                 (expand-file-name "cook/Cookbook.py" as-dir)))))
    (if (file-remote-p book)
        (tramp-file-name-localname
         (tramp-dissect-file-name book))
      book)))

(defun cook-slurp (f)
  "Read contents of file F."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun cook-recompile ()
  "Wrap around `recompile'.
This command expects to be bound to \"g\" in `comint-mode'."
  (interactive)
  (if (or (and (get-buffer-process (current-buffer))
               (equal (this-command-keys) "g"))
          (cook-editing-command-p))
      (self-insert-command 1)
    (cook--recompile)))

(defun cook--recompile ()
  (let* ((dd default-directory)
         (cmd (cond
               ((save-excursion
                  (beginning-of-line)
                  (looking-at "cook "))
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
               ((string-match "\\`\\*compile  \\(.*\\)\\*\\'" (buffer-name))
                (match-string-no-properties 1 (buffer-name)))))
         (old-process (get-buffer-process (current-buffer))))
    (when old-process
      (kill-process old-process))
    (let ((new-name (concat "*compile  " cmd "*")))
      (rename-buffer new-name)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((default-directory dd))
      (cook--run cmd))))

(defun cook-current-cmd ()
  "Get the cook shell command from the current `comint-mode' buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line 3)
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(defun cook-editing-command-p ()
  "Check if we're editing `cook-current-cmd'."
  (and
   (eq major-mode 'comint-mode)
   (save-excursion
     (goto-char (point-min))
     (looking-at "-\\*- mode: compilation;"))
   (= 4 (line-number-at-pos))))

(defun cook-reselect ()
  "Select a different recipe from the current cookbook.
Or forward to `self-insert-command'."
  (interactive)
  (if (or (get-buffer-process (current-buffer))
          (cook-editing-command-p))
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
    (define-key map (kbd "<return>") 'cook-recompile-or-send-input)
    (define-key map (kbd "<backtab>") 'cook-next-field)
    (define-key map (kbd "C-r") 'counsel-shell-history)
    map)
  "Keymap for `cook-comint-mode'.")

(define-minor-mode cook-comint-mode
  "Minor mode for `comint-mode' buffers produced by `compile'."
  :keymap cook-comint-mode-map
  :group 'cook
  (if cook-comint-mode
      (setq comint-scroll-to-bottom-on-output t)))

(defun cook-next-field ()
  "Goto the next argument field.
See `cook-editing-command-p'."
  (interactive)
  (if (= 4 (line-number-at-pos))
      (unless (search-forward " '" (line-end-position) t)
        (beginning-of-line)
        (search-forward " '" (line-end-position) t))
    (goto-char (point-min))
    (forward-line 3)))

(defun cook-recompile-or-send-input ()
  "Recompile when on line 4, else `comint-send-input'."
  (interactive)
  (if (cook-editing-command-p)
      (cook--recompile)
    (comint-send-input)))

(defvar cook-history nil
  "History for `cook'.")

(defvar cook-arg-history nil
  "History for `cook' args.")

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
  (let ((window (cl-find-if
                 (lambda (w) (eq (window-buffer w) buffer))
                 (window-list))))
    (when window
      (select-window window)
      buffer)))

(defvar cook-last-recipe ""
  "Store the last recipe.")

(defun cook-script (&rest args)
  "The script to run pycook on ARGS."
  (apply #'concat
         (if (file-remote-p default-directory)
             "python3 -m pycook"
           "cook")
         args))

(defun cook-global-recipes ()
  (mapcar
   #'file-name-base
   (split-string
    (shell-command-to-string
     (cook-script " :"))
    "\n" t)))

;;;###autoload
(defun cook (&optional arg recipe)
  "Locate Cookbook.py in the current project and run RECIPE.

When ARG is non-nil, open Cookbook.py instead."
  (interactive "P")
  (cond ((equal arg '(16))
         (find-file (cook-current-cookbook)))
        ((window-minibuffer-p))
        (arg
         (ivy-read "book: "
                   (cook-global-recipes)
                   :action (lambda (module)
                             (cook-book (concat ":" module) recipe))
                   :require-match t
                   :caller 'cook))
        (t
         (when (buffer-file-name)
           (save-buffer))
         (cook-book
          (cook-current-cookbook) recipe))))

(defun cook-action-find-file (module)
  "Open MODULE in Emacs."
  (let ((help (shell-command-to-string
               (cook-script " :" module))))
    (if (string-match "^Book: \\(.*\\)$" help)
        (find-file (match-string 1 help))
      (error "Could not find book name in '%s'" help))))

(ivy-set-actions
 'cook
 '(("f" cook-action-find-file "find-file")))

(defun cook--parse-args (spec)
  (let ((args (cdr (split-string spec ":" t " ")))
        (res nil))
    (dolist (arg args)
      (let (arg-name arg-def)
        (if (string-match "\\`\\(\\(?:\\sw\\|\\s_\\)+\\)=\\(.*\\)\\'" arg)
            (setq arg-name (match-string 1 arg)
                  arg-def (match-string 2 arg))
          (setq arg-name arg))
        (push (cons arg-name
                    (if (string-match "\\`(.*)\\'" arg-def)
                        (split-string (substring arg-def 1 -1) "|")
                      (string-trim arg-def "'" "'")))
              res)))
    (nreverse res)))

(defun cook--read-args (args-spec)
  (let ((res nil))
    (dolist (arg args-spec)
      (let ((arg-name (car arg))
            (arg-options (cdr arg)))
        (unless (member arg-name '("logname" "vterm"))
          (let* ((prompt (concat arg-name ": "))
                 (arg-val
                  (cond
                   ((string= arg-name "fname")
                    (ivy-read prompt #'read-file-name-internal
                              :history 'cook-arg-history))
                   ((stringp arg-options)
                    (read-from-minibuffer
                     prompt
                     arg-options nil nil 'cook-arg-history))
                   (t
                    (ivy-read prompt arg-options
                              :history 'cook-arg-history)))))
            (push (format ":%s %s" arg-name (shell-quote-argument arg-val)) res)))))
    (nreverse res)))


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
                         (concat cook-cmd " --list")) "\n" t " "))
         (recipes-alist
          (mapcar (lambda (s) (cons (car (split-string s " :")) s)) recipes))
         (args-spec nil)
         (recipe
          (or
           recipe
           (let* ((recipe (setq cook-last-recipe
                                (ivy-read "recipe: " recipes-alist
                                          :preselect cook-last-recipe
                                          :require-match t
                                          :history 'cook-history
                                          :caller 'cook-book)))
                  (spec (cdr (assoc recipe recipes-alist)))
                  (args (cook--read-args (setq args-spec (cook--parse-args spec)))))
             (mapconcat #'identity (cons recipe args) " "))))
         (cmd
          (concat cook-cmd " " recipe))
         buf)
    (cook--run cmd (assoc "vterm" args-spec))))

(defun cook--run (cmd &optional vterm)
  (if (require 'mash nil t)
      (let* ((reuse-buffer
              (and (eq major-mode 'comint-mode)
                   (null (get-buffer-process (current-buffer)))))
             (name (format "*compile  %s*" cmd))
             (buf (get-buffer name)))
        (if (and buf
                 (if (process-live-p (get-buffer-process buf))
                     t
                   (switch-to-buffer buf)
                   (setq reuse-buffer t)
                   nil))
            (or
             (cook-select-buffer-window buf)
             (switch-to-buffer buf))
          (setq buf (mash-make-shell
                     cmd
                     (if vterm
                         'mash-new-vterm
                       'mash-new-compilation)
                     cmd reuse-buffer))
          (with-current-buffer buf
            (unless vterm
              (cook-comint-mode)
              (goto-address-mode)))
          (cook-select-buffer-window buf)))
    (let ((new-name (concat "*compile  " cmd "*")))
      (switch-to-buffer
       (get-buffer-create new-name))
      (advice-add 'compilation-sentinel :after #'cook--input-sentinel)
      (with-current-buffer (compile cmd t)
        (cook-comint-mode)))))

(provide 'cook)

;;; cook.el ends here
