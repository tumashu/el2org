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

;; 1. `el2org-orgify-if-necessary' can convert an elisp file to org-file.
;; 2. `el2org-generate-readme' can generate README.md from elisp's "Commentary"
;;     section.

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

(defun el2org-orgify-if-necessary (el-file &optional force)
  (let ((org-file (concat (file-name-sans-extension el-file) ".org")))
    (when (or force
              (not (file-exists-p org-file))
              (file-newer-than-file-p el-file org-file))
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
        ;; Delete ";;"
        (let ((positions '()))
          (goto-char (point-min))
          (let ((status t))
            (while status
              (thing-at-point--end-of-sexp)
              (push (point) positions)
              (unless (< (point) (point-max))
                (setq status nil))))
          (goto-char (point-max))
          (let ((status t))
            (while status
              (thing-at-point--beginning-of-sexp)
              (if (> (point) (point-min))
                  (push (point) positions)
                (setq status nil))))
          (push (point-min) positions)
          (push (point-max) positions)
          (setq positions (sort positions #'<))
          (let (p1 p2)
            (while positions
              (setq p1 (pop positions))
              (setq p2 (pop positions))
              (while (re-search-forward "^;; " p2 t)
                (replace-match "" nil t)))))
        ;; Delete Useless "BEGIN/END_SRC"
        (goto-char (point-min))
        (while (re-search-forward "^#[+]END_SRC\n#[+]BEGIN_SRC[ ]+emacs-lisp\n" nil t)
          (replace-match "" nil t))
        ;; Deal with first line prefix with ";;;"
        (goto-char (point-min))
        (while (re-search-forward "^;;;[ ]+" (line-end-position) t)
          (replace-match "#+TITLE: " nil t))
        ;; Delete ";;;###autoload"
        (goto-char (point-min))
        (while (re-search-forward "^;;;###autoload.*" nil t)
          (replace-match "" nil t))
        ;; Delete ";;; Commentary:"
        (goto-char (point-min))
        (while (re-search-forward "^;;; +Commentary:.*" nil t)
          (replace-match "" nil t))
        ;; Delete ";;; Code:"
        (goto-char (point-min))
        (while (re-search-forward "^;;; +Code:.*" nil t)
          (replace-match "" nil t))
        (write-file org-file)))
    org-file))

(defun el2org-generate-file (directory el-filename tags backend output-filename)
  (let* ((el-file (concat (file-name-as-directory directory) el-filename))
         (output-file (concat (file-name-as-directory directory) output-filename))
         (org-file (el2org-orgify-if-necessary el-file)))
    (when (and (file-exists-p el-file)
               (file-exists-p org-file))
      (with-temp-buffer
        (insert-file-contents org-file)
        (org-mode)
        (let ((org-export-select-tags tags)
              (org-export-with-tags nil)
              (indent-tabs-mode nil)
              (tab-width 4))
          (org-export-to-file backend output-file))))))

;;;###autoload
(defun el2org-generate-readme ()
  "Generate README.md from current emacs-lisp file."
  (interactive)
  (let* ((file (buffer-file-name))
         (filename (when file
                     (file-name-nondirectory file)))
         (directory (when file
                      (file-name-directory file))))
    (when (and file (string-match-p "\\.el$" file)
               filename directory)
      (el2org-generate-file
       directory filename '("README")
       (if (featurep 'ox-gfm)
           'gfm
         (message "Can't generate README.md with ox-gfm, use ox-md instead!")
         'md)
       "README.md"))))

(provide 'el2org)

;; * Footer

;;; el2org.el ends here
