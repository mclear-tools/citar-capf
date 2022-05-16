;;; citar-capf.el --- bibtex completation at point -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Created: May 15, 2022
;; Version: 0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1") (parsebib "3.0") (org "9.5") (citeproc "0.9"))
;; Homepage: https://github.com/mclear-tools/citar-capf
;; Keywords: convenience, bibtex


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
Parsing by citar is of bibtex, biblatex and csl json files, and
supports multiple different files. Please see citar's
documentation for further info."
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
