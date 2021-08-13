(add-to-list 'load-path (file-name-directory (file-chase-links load-file-name)))
(require 'elpa)

;;* `cs-org-to-pdf'
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

(defun cs-org-to-pdf (fname)
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

(defun cs-org-to-html (fname)
  (require 'ox-html)
  (require 'htmlize)
  (save-window-excursion
    (find-file fname)
    (org-html-export-to-html))
  (run-with-idle-timer
   0 nil
   (lambda ()
     (when (eq major-mode 'dired-mode)
       (revert-buffer)
       (dired-goto-file
        (expand-file-name (replace-regexp-in-string "org$" "html" fname)))))))

(defun cs-replace-all (from to)
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to)))

(defun cs-md-cleanup (fname)
  (with-current-buffer (find-file-noselect fname)
    (cs-replace-all "^-  " "- ")
    (cs-replace-all "^ +:" ":")
    (cs-replace-all "\n\\{3,\\}" "\n\n")
    ;; non-breakable space
    (cs-replace-all " " " ")
    (goto-char (point-min))
    (while (eq (forward-paragraph) 0)
      (fill-paragraph))
    (save-buffer)))
