;;; el2org.el --- Convert elisp file to org file

;; * Header
;; Copyright (C) 2017 Feng Shu

;; Author: Feng Shu  <tumashu AT 163.com>
;; Homepage: https://github.com/tumashu/el2org
;; Keywords: convenience
;; Version: 0.10

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * What is el2org                                      :README:
;; el2org is a simple tool，which can convert a emacs-lisp file to org file.
;; You can write code and document in a elisp file with its help.

;; #+BEGIN_EXAMPLE
;;            (convert to)                    (export to)
;; elisp  -----------------> org (internal) --------------> other formats
;; #+END_EXAMPLE

;; Note: el2org.el file may be a good example.

;; ** Installation

;; 1. Config melpa source, please read: http://melpa.org/#/getting-started
;; 2. M-x package-install RET el2org RET
;; 3. M-x package-install RET ox-gfm RET

;;    ox-gfm is needed by `el2org-generate-readme', if ox-gfm can not be found,
;;    ox-md will be used as fallback.

;; ** Configure

;; #+BEGIN_EXAMPLE
;; (require 'el2org)
;; (require 'ox-gfm)
;; #+END_EXAMPLE

;; ** Usage

;; 1. `el2org-generate-file' can convert an elisp file to other file format
;;     which org's exporter support.
;; 2. `el2org-generate-readme' can generate README.md from elisp's "Commentary"
;;     section.
;; 3. `el2org-generate-html' can generate a html file from current elisp file
;;    and browse it.
;; 4. `el2org-generate-org' can generate a org file from current elisp file.

;;; Code:

;; * el2org's code                                                        :code:
(require 'lisp-mode)
(require 'thingatpt)
(require 'org)
(require 'ox)

(defvar el2org-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c.." 'el2org-generate-readme)
    keymap)
  "Keymap for `el2org-mode'")

(define-minor-mode el2org-mode
  "Minor for el2org."
  nil " el2org" 'el2org-mode-map)

(defun el2org-generate-file (el-file tags backend output-file &optional force with-tags)
  (when (and (string-match-p "\\.el$" el-file)
             (file-exists-p el-file)
             (or force
                 (not (file-exists-p output-file))
                 (file-newer-than-file-p el-file output-file)))
    (with-temp-buffer
      (insert-file-contents el-file)
      (emacs-lisp-mode)
      ;; Add "#+END_SRC"
      (goto-char (point-min))
      (let ((status t))
        (while status
          (thing-at-point--end-of-sexp)
          (unless (< (point) (point-max))
            (setq status nil))
          (insert "\n;; #+END_SRC")))
      ;; Add "#+BEGIN_SRC"
      (goto-char (point-max))
      (let ((status t))
        (while status
          (thing-at-point--beginning-of-sexp)
          (if (> (point) (point-min))
              (insert ";; #+BEGIN_SRC emacs-lisp\n")
            (setq status nil))))
      ;; Delete useless "BEGIN_SRC/END_SRC"
      (goto-char (point-min))
      (while (re-search-forward "^;; #[+]END_SRC\n;; #[+]BEGIN_SRC[ ]+emacs-lisp\n" nil t)
        (replace-match "" nil t))
      ;; Deal with first line if it prefix with ";;;"
      (goto-char (point-min))
      (while (re-search-forward "^;;;[ ]+" (line-end-position) t)
        (replace-match ";; #+TITLE: " nil t))
      ;; Indent the buffer, so ";;" and ";;;" in sexp will not be removed.
      (indent-region (point-min) (point-max))
      ;; Deal with ";;"
      (goto-char (point-min))
      (while (re-search-forward "^;;[ ]" nil t)
        (replace-match "" nil t))
      ;; Deal with ";;;"
      (goto-char (point-min))
      (while (re-search-forward "^;;;" nil t)
        (replace-match "# ;;;" nil t))
      ;; Export
      (org-mode)
      (let ((org-export-select-tags tags)
            (org-export-with-tags with-tags)
            (indent-tabs-mode nil)
            (tab-width 4))
        (org-export-to-file backend output-file))
      output-file)))

;;;###autoload
(defun el2org-generate-readme ()
  "Generate README.md from current emacs-lisp file."
  (interactive)
  (let* ((file (buffer-file-name))
         (readme-file (concat (file-name-sans-extension file) ".md")))
    (el2org-generate-file
     file '("README")
     (if (featurep 'ox-gfm)
         'gfm
       (message "Can't generate README.md with ox-gfm, use ox-md instead!")
       'md)
     readme-file t)))

;;;###autoload
(defun el2org-generate-html ()
  "Generate html file from current elisp file and browse it."
  (interactive)
  (let* ((file (buffer-file-name))
         (html-file (concat (make-temp-file "el2org-") ".html")))
    (el2org-generate-file file nil 'html html-file t)
    (browse-url-default-browser html-file)))

;;;###autoload
(defun el2org-generate-org ()
  "Generate org file from current elisp file."
  (interactive)
  (let* ((file (buffer-file-name))
         (org-file (concat (file-name-sans-extension file) ".org")))
    (el2org-generate-file file nil 'org org-file t t)))

(provide 'el2org)

;; * Footer

;;; el2org.el ends here
