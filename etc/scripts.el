;;* ELPA
(setq melpa-stable (getenv "MELPA_STABLE"))

(defun cook-elpa ()
  (setq package-user-dir
        (expand-file-name
         (format "~/.elpa/%s/elpa"
                 (concat emacs-version (when melpa-stable "-stable")))))
  (package-initialize)
  (add-to-list 'load-path default-directory))

(defun cook-install-deps (dev-packages)
  (cook-elpa)
  (message "installing in %s ...\n" package-user-dir)
  (setq package-archives
        (list (if melpa-stable
                  '("melpa-stable" . "https://stable.melpa.org/packages/")
                '("melpa" . "http://melpa.org/packages/"))
              '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-refresh-contents)
  (dolist (package dev-packages)
    (if (package-installed-p package)
        (message "%S: OK" package)
      (condition-case nil
          (progn
            (package-install package)
            (message "%S: ...OK" package))
        (error
         (message "%S: FAIL" package)))))
  (save-window-excursion
    (package-list-packages t)
    (condition-case nil
        (progn
          (package-menu-mark-upgrades)
          (package-menu-execute t))
      (error
       (message "All packages up to date")))))

(defun cook-byte-compile (&rest fnames)
  (cook-elpa)
  (setq byte-compile--use-old-handlers nil)
  (dolist (fname fnames)
    (batch-byte-compile-file fname)))

;;* `org-to-pdf'
(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t ("pdflatex"))
        ("T1" "fontenc" t ("pdflatex"))
        ("" "graphicx" t)
        ("" "tabularx" t)
        ("" "grffile" t)
        ("" "minted" nil)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "enumitem" nil)
        ("hidelinks,colorlinks=true,linkcolor=blue,urlcolor=blue" "hyperref" nil)))
(setq org-export-with-sub-superscripts nil)
(setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
(setq org-latex-tables-centered nil)
(setq org-descriptive-links nil)
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun org-to-pdf (fname)
  (require 'ox-latex)
  (find-file fname)
  (condition-case nil
      (org-latex-export-to-pdf)
    (error
     (with-current-buffer "*Org PDF LaTeX Output*"
       (write-file "/tmp/error.txt"))))
  (with-current-buffer "*Org PDF LaTeX Output*"
    (goto-char (point-min))
    (when (re-search-forward "^!" nil t)
      (error (buffer-substring-no-properties
              (line-beginning-position)
              (line-end-position))))))
