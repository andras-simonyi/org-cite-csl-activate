;;; oc-csl-activate.el --- CSL-based citation activation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 András Simonyi
;; Maintainer: András Simonyi <andras.simonyi@gmail.com>
;; URL: https://github.com/andras-simonyi/org-cite-csl-activate
;; Keywords: csl, org-mode, cite, bib
;; Package-Requires: ((emacs "25.1") (org "9.5") (citeproc "0.9")
;; Version: 0.0.1

;; Author: András Simonyi <andras.simonyi@gmail.com>

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This library provides an (experimental) org-cite activation processor that
;; fontifies Org citations by rendering them with the citeproc-el CSL citation
;; processor. The processor is registered under the name `csl-activate'.

;;; Code:

(require 'citeproc)
(require 'org-element)
(require 'ox)
(require 'oc)
(require 'oc-csl)

(defvar org-cite-csl-activate-use-document-style nil
  "Whether to use the citation style of the current document.
When nil, `org-cite-csl--fallback-style-file' is always used.")


;;; Internal variables and functions

(defvar org-cite-csl-activate--processor-cache nil
  "Cache for the citation processor.")

(make-variable-buffer-local 'org-cite-csl-activate--processor-cache)

(defun org-cite-csl-activate--processor ()
  "Return a `citeproc-el' processor for activation."
  (or org-cite-csl-activate--processor-cache
      (let* ((bibliography (org-cite-list-bibliography-files))
	     (cite-string (cadar (org-collect-keywords '("CITE_EXPORT"))))
	     (cite-spec (when (stringp cite-string) (split-string cite-string "[ \t]")))
	     (csl-style (if (and cite-spec (string= "csl" (car cite-spec)) (cdr cite-spec))
			    (expand-file-name (cadr cite-spec) org-cite-csl-styles-dir)
			  org-cite-csl--fallback-style-file))
	     (processor
	      (citeproc-create
	       (if org-cite-csl-activate-use-document-style
                   csl-style
                 org-cite-csl--fallback-style-file)
	       (citeproc-hash-itemgetter-from-any bibliography)
	       (org-cite-csl--locale-getter))))
	(setq org-cite-csl-activate--processor-cache processor)
	processor)))

(defun org-cite-csl-activate--get-item (key)
  "Return item data with KEY from the bibliography.
Return nil if KEY is not found."
  (let* ((proc (org-cite-csl-activate--processor))
	 (item-getter (citeproc-proc-getter proc))
	 (result (funcall item-getter (list key))))
    (alist-get key result)))

(defun org-cite-csl-activate--fontify-rendered (citation beg end)
  "Fontify CITATION with boundaries BEG END by rendering it."
  (let* ((rendered-cit-struct (get-text-property beg 'rendered-cit-struct))
	 (proc (org-cite-csl-activate--processor))
	 (info (list :cite-citeproc-processor proc))
	 (act-cit-struct (org-cite-csl--create-structure citation info))
	 rendered-cite rendered-bib)
    (if  (equal rendered-cit-struct act-cit-struct)
	(setq  rendered-cite (get-text-property beg 'rendered-cite)
	       rendered-bib (get-text-property beg 'rendered-bib))
      ;; Re-render if the citation structure changed
      (citeproc-clear proc)
      (put-text-property beg end 'rendered-cit-struct (copy-sequence act-cit-struct))
      (citeproc-append-citations (list act-cit-struct) proc)
      (setq rendered-cite (car (citeproc-render-citations proc 'plain t)))
      (setq rendered-bib (car (citeproc-render-bib proc 'plain nil)))
      (put-text-property beg end 'rendered-cite rendered-cite)
      (put-text-property beg end 'rendered-bib rendered-bib)
      (put-text-property beg end 'citation-object citation))
    ;; Display rendered cite and bib
    (put-text-property beg end 'display rendered-cite)
    (put-text-property beg end 'help-echo rendered-bib)))

(defun org-cite-csl-activate--sensor-fun (_ prev motion)
  "Cursor sensor function for activated citations."
  (let* ((pos (if (eq motion 'left) prev (point)))
	 (citation (get-text-property pos 'citation-object)))
    (pcase-let ((`(,beg . ,end) (org-cite-csl-activate--get-boundaries pos)))
      (if (eq motion 'left)
	  (org-cite-csl-activate--fontify-rendered citation beg end)
	(put-text-property beg end 'display nil)))))


;;; Utilities 

(defun org-cite-csl-activate--get-boundaries (pos)
    "Return the beginning and end of a citation containing POS.
Returns a (BEG . END) pair."
  (save-excursion
    (let (end)
      (goto-char pos)
      (setq end (search-forward "]"))
      (cons (search-backward "[") end))))


;;; Main entry points 

(defun org-cite-csl-activate-render-all ()
  "Fontify all citations in the buffer by rendering them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-element-citation-prefix-re nil t)
      (let ((citation (org-element-property :parent (org-element-context))))
	(pcase-let ((`(,beg . ,end) (org-cite-csl-activate--get-boundaries (+ 2 (point)))))
	  (org-cite-csl-activate--fontify-rendered citation beg end))))))

;;; Activation function 
(defun org-cite-csl-activate (citation)
  "Fontify CITATION object by rendering it with `citeproc-el'."
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    (put-text-property beg end font-lock-multiline t)
    (add-face-text-property beg end 'org-cite)
    (put-text-property beg end 'citation-object citation)
    (let ((all-keys-found t))
      (dolist (reference (org-cite-get-references citation))
	(let ((boundaries (org-cite-key-boundaries reference)))
	  (add-face-text-property
	   (car boundaries) (cdr boundaries)
	   (if (org-cite-csl-activate--get-item (org-element-property :key reference))
	       'org-cite-key
	     (setq all-keys-found nil)
	     'error))))
      (if all-keys-found
	  (progn
	    (put-text-property beg end 'cursor-sensor-functions
			       (list #'org-cite-csl-activate--sensor-fun))
	    (put-text-property (- end 1) end 'rear-nonsticky
			       '(cursor-sensor-functions)))
	(put-text-property beg end 'cursor-sensor-functions nil)
	(put-text-property beg end 'display nil)
	(put-text-property beg end 'help-echo nil)))))


;;; Register the activation processor
(org-cite-register-processor 'csl-activate 
  :activate #'org-cite-csl-activate)

(provide 'org-cite-csl-activate)
(provide 'oc-csl-activate)
;;; oc-csl-activate.el ends here
