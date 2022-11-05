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

(defun cs-md-cleanup (fname-md)
  (let ((fname-org (concat (file-name-sans-extension fname-md) ".org")))
    (with-current-buffer (find-file-noselect fname-org)
      (cs-replace-all "^-  " "- ")
      (cs-replace-all "^ +:" ":")
      (cs-replace-all "\n\\{3,\\}" "\n\n")
      ;; non-breakable space
      (cs-replace-all " " " ")

      (goto-char (point-min))
      (while (or (bolp) (outline-next-heading))
        (end-of-line)
        (when (looking-at "\n *:PROPERTIES:")
          (let ((beg (point)))
            (goto-char (match-end 0))
            (search-forward ":END:" nil t)
            (skip-chars-forward "\n")
            (backward-char)
            (delete-region beg (point)))))

      ;; make all links one-line
      (goto-char (point-min))
      (while (search-forward "][" nil t)
        (let ((beg (point)))
          (while (not (search-forward "]]" (line-end-position) t))
            (end-of-line)
            (delete-char 1)
            (just-one-space))))

      (goto-char (point-min))
      (while (eq (forward-paragraph) 0)
        (fill-paragraph))
      (save-buffer))))


(require 'ol)
(org-link-set-parameters "roam" :export #'orly--roam-export)

(defun orly--roam-export (path desc format)
  (cond ((eq format 'html)
         (format "<a href=\"https://www.google.com/search?q=%s\">[%s]</a>"
                 (url-hexify-string desc)
                 desc))
        ((memq format '(md gfm))
         (format "[%s](https://www.google.com/search?q=%s)"
                 desc
                 (url-hexify-string desc)))
        (t
         (format "[%s]" desc))))

(defun cs-org-to-md (fname)
  (require 'ox-md)
  (find-file fname)
  (org-md-export-to-markdown))

(defun cs-org-to-gfm (fname)
  (require 'ox-gfm)
  (find-file fname)
  (org-gfm-export-to-markdown))
