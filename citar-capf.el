;;; citar-capf.el --- bibtex completation at point -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: version
;; Package-Requires: (citar "0.9")
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Completion at point for citations using citar as its backend.

;;; Code:
;;;; Requirements
(require 'citar)
(require 'org)

(declare-function org-element-at-point "org-element")
(declare-function citar--get-candidates "citar")

;;;; Functions
(defun citar-capf ()
  "Complete citation key at point for org, markdown, or latex.
The parsing of the bibtex file is handled by citar."
  (let ((element (org-element-at-point))
        (capf-citar-latex-regexp "\\(?:cite\\(?:\\(?:[pt]\\*\\|[pt]\\)?{\\)\\)\\([[:alnum:]_-]*,\\)*\\([[:alnum:]_-]*\\)")
        (capf-citar-markdown-regexp (concat "-?@"                         ; @ preceded by optional -
                                            "\\(?:"
                                            "{\\(?1:.*?\\)}"              ; brace-delimited key
                                            "\\|"
                                            "\\(?1:[[:alnum:]_][[:alnum:]]*\\(?:[:.#$%&+?<>~/-][[:alnum:]]+\\)*\\)"
                                            "\\)")))

    ;; only list candidates in certain contexts
    (when

        ;; conditional recognition of citation key by mode
        (cond

         ;; latex-mode
         ((derived-mode-p 'latex-mode)
          (looking-back capf-citar-latex-regexp 2))

         ;; org-mode
         ((and (derived-mode-p 'org-mode)
               (or (eq 'citation (org-element-type (org-element-context element)))
                   (and (or (eq ?@ (char-before))
                            (looking-back org-element-citation-key-re
                                          (line-beginning-position)))
                        (let ((origin (point)))
                          (save-excursion
                            (and (re-search-backward org-element-citation-prefix-re
                                                     (org-element-property
                                                      :begin element)
                                                     t)
                                 (not (search-forward "]" origin t)))))))))
         ;; markdown-mode
         ((and (derived-mode-p 'markdown-mode)
               (or (eq ?@ (char-before))
                   (looking-back capf-citar-markdown-regexp
                                 (line-beginning-position))))))

      ;; Get and insert candidate
      (let* ((candidates (citar--get-candidates))
             (begin (save-excursion (backward-word) (point)))
             (end (point)))
        (list begin end candidates
              :exclusive 'no
              :exit-function
              (lambda (str _status)
                ;; take completion str and replace with key
                (delete-char (- (length str)))
                (insert (cadr (assoc str candidates)))))))))

(provide 'citar-capf)
;;; citar-capf.el ends here
