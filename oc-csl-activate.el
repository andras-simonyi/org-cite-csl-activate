;;; oc-csl-activate.el --- CSL-based citation activation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 András Simonyi
;; Maintainer: András Simonyi <andras.simonyi@gmail.com>
;; URL: https://github.com/andras-simonyi/org-cite-csl-activate
;; Keywords: csl, org-mode, cite, bib
;; Package-Requires: ((emacs "25.1") (org "9.5") (citeproc "0.9"))
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

(require 'citar nil t)
(declare-function citar-get-entry "ext:citar")


;;; Variables

(defgroup org-cite-csl-activate nil
  "Options for CSL-based citation activation."
  :group 'org-cite)

(defcustom org-cite-csl-activate-use-document-style nil
  "Whether to use the citation style of the current document.
When nil, `org-cite-csl--fallback-style-file' is always used."
  :type 'boolean
  :safe 'booleanp)

(defcustom org-cite-csl-activate-use-document-locale nil
  "Whether to use the locale of the current document.
When nil, the fallback of en-US is used."
  :type 'boolean
  :safe 'booleanp)

(defcustom org-cite-csl-activate-use-citar-cache nil
  "Whether to use the Citar cache for retrieving bibliography items.
When nil, the default Citeproc itemgetter function is used."
  :type 'boolean
  :safe 'booleanp)

;; Internal

(defvar-local org-cite-csl-activate--processor-cache nil
  "Cache for the citation processor.")


;;; Utilities

(defun org-cite-csl-activate--get-boundaries (pos)
  "Return the beginning and end of a citation containing POS.
Returns a (BEG . END) pair."
  (save-excursion
    (let (end)
      (goto-char pos)
      (setq end (search-forward "]" nil t))
      (cons (search-backward "[" nil t) end))))

(defun org-cite-csl-activate--get-citation (pos)
  "Return the citation object at buffer position POS.
Return nil if there is no citation object at the position."
  (let ((element (org-with-point-at pos (org-element-context))))
    (when (memq (car element) '(citation citation-reference))
      (when (eq (car element) 'citation-reference)
	(setq element (org-element-property :parent element)))
      element)))

(defun org-cite-csl-activate--cslize-special-vars (entry)
  "Convert bibtex format name and date field values in ENTRY to CSL."
  (mapcar
   (pcase-lambda (`(,var . ,value))
     (cons var
	   (cond ((memq var citeproc--date-vars) (citeproc-bt--to-csl-date value nil))
		 ((memq var citeproc--name-vars) (citeproc-bt--to-csl-names value))
		 (t value))))
   entry))

(defun org-cite-csl-activate--csl-from-citar-entry (entry)
  "Return a CSL version of Citar ENTRY."
  (pcase (caar entry)
    ('nil nil)
    ;; If keys are strings then it is a bib(la)tex entry, which has to be converted
    ;; to CSL.
    ((pred stringp) (citeproc-blt-entry-to-csl entry))
    ;; Symbol keys indicate CSL entries, only special vars are converted.
    ((pred symbolp) (org-cite-csl-activate--cslize-special-vars entry))
    (_ (error "Bib entry with unknown format: %s" entry))))

(defun org-cite-csl-activate--citar-itemgetter (keys)
  "Return itemdata for KEYS from the citar cache."
  (mapcar
   (lambda (key)
     (let ((citar-entry (citar-get-entry key)))
       (cons key (org-cite-csl-activate--csl-from-citar-entry citar-entry))))
   keys))

(defun org-cite-csl-activate--check-citar ()
  "Raise an error if Citar is not loaded."
  (unless (featurep 'citar)
    (error "Citar is not loaded")))


;;; Internal functions

(defun org-cite-csl-activate--processor ()
  "Return a `citeproc-el' processor for activation."
  (or org-cite-csl-activate--processor-cache
      (let* ((processor
	      (citeproc-create
               (or (when org-cite-csl-activate-use-document-style
                     (let* ((cite-string (cadar (org-collect-keywords '("CITE_EXPORT"))))
	                    (cite-spec (when (stringp cite-string) (split-string cite-string "[ \t]"))))
                       (when (and cite-spec (string= "csl" (car cite-spec)) (cdr cite-spec))
			 (expand-file-name (cadr cite-spec) org-cite-csl-styles-dir))))
                   org-cite-csl--fallback-style-file)
	       (if org-cite-csl-activate-use-citar-cache
		   (progn
		    (org-cite-csl-activate--check-citar)
		    #'org-cite-csl-activate--citar-itemgetter)
		 (citeproc-hash-itemgetter-from-any (org-cite-list-bibliography-files)))
	       (org-cite-csl--locale-getter)
               (when org-cite-csl-activate-use-document-locale
                 (cadar (org-collect-keywords '("language")))))))
	(setq org-cite-csl-activate--processor-cache processor)
	processor)))

(defun org-cite-csl-activate--get-item (key)
  "Return item data with KEY from the bibliography.
Return nil if KEY is not found."
  (let* ((proc (org-cite-csl-activate--processor))
	 (item-getter (citeproc-proc-getter proc))
	 (result (funcall item-getter (list key))))
    (cdar result)))

(defun org-cite-csl-activate--citaton-keys-valid-p (citation)
  "Return non-nil when all keys of CITATION are valid, nil otherwise."
  (let ((references (org-cite-get-references citation))
	(all-keys-valid t))
    (while (and references all-keys-valid)
      (when (not (org-cite-csl-activate--get-item
		  (org-element-property :key (pop references))))
	(setq all-keys-valid nil)))
    all-keys-valid))

(defun org-cite-csl-activate--fontify-rendered (citation beg end)
  "Fontify CITATION with boundaries BEG END by rendering it."
  (when (and citation
	     (not (member (car (org-cite-citation-style citation nil)) '("nocite" "n")))
	     (org-cite-csl-activate--citaton-keys-valid-p citation))
   (with-silent-modifications
     (let* ((rendered-cit-struct (get-text-property beg 'rendered-cit-struct))
	    (proc (org-cite-csl-activate--processor))
	    (info (list :cite-citeproc-processor proc))
	    (act-cit-struct (org-cite-csl--create-structure citation info))
	    rendered-cite)
       (if (equal rendered-cit-struct act-cit-struct)
	   (setq rendered-cite (get-text-property beg 'rendered-cite))
	 ;; Re-render if the citation structure changed
	 (citeproc-clear proc)
	 (put-text-property beg end 'rendered-cit-struct (copy-sequence act-cit-struct))
	 (citeproc-append-citations (list act-cit-struct) proc)
	 (setq rendered-cite (car (citeproc-render-citations proc 'plain t)))
	 (put-text-property beg end 'rendered-cite rendered-cite)
	 ;; Invalidate the rendered bib cache
	 (put-text-property beg end 'rendered-bib nil))
       ;; Display rendered cite and bib
       (put-text-property beg end 'display rendered-cite)
       (put-text-property beg end 'help-echo #'org-cite-csl-activate--help-echo-fun)))))

(defun org-cite-csl-activate--help-echo-fun (_ buffer pos)
  "Help-echo function for activated citations."
  (with-current-buffer buffer
   (if-let ((cached-rendered-bib (get-text-property pos 'rendered-bib)))
       cached-rendered-bib
     (when-let  ((citation (org-cite-csl-activate--get-citation pos)))
       (let* ((proc (org-cite-csl-activate--processor))
	      (info (list :cite-citeproc-processor proc))
	      (cit-struct (org-cite-csl--create-structure citation info))
	      rendered-bib)
	 (citeproc-clear proc)
	 (citeproc-append-citations (list cit-struct) proc)
	 (setq rendered-bib (car (citeproc-render-bib proc 'plain nil)))
	 (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
	   (with-silent-modifications
	     (put-text-property beg end 'rendered-bib rendered-bib)))
	 rendered-bib)))))

(defun org-cite-csl-activate--sensor-fun (_ prev motion)
  "Cursor sensor function for activated citations."
  (let* ((pos (if (eq motion 'left) prev (point)))
	 (element (org-cite-csl-activate--get-citation pos)))
    (pcase-let ((`(,beg . ,end) (org-cite-csl-activate--get-boundaries pos)))
      (when (and beg end)
       (if (eq motion 'left)
	   (org-cite-csl-activate--fontify-rendered element beg end)
	 (with-silent-modifications
	   (put-text-property beg end 'display nil)))))))


;;; Main entry points 

(defun org-cite-csl-activate-render-all ()
  "Fontify all citations in the buffer by rendering them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-element-citation-prefix-re nil t)
      (let ((parent (org-element-property :parent (org-element-context))))
	(when (and (eq (org-element-type parent) 'citation)
		   (not (member (car (org-cite-citation-style parent nil))
				'("n" "nocite"))))
	  (pcase-let
	      ((`(,beg . ,end) (org-cite-csl-activate--get-boundaries (+ 2 (point)))))
	    (when (and beg end)
	     (org-cite-csl-activate--fontify-rendered parent beg end))))))))

;;; Activation function 
(defun org-cite-csl-activate (citation)
  "Fontify CITATION object by rendering it with `citeproc-el'."
  (with-silent-modifications
    (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
      (put-text-property beg end font-lock-multiline t)
      (add-face-text-property beg end 'org-cite)
      (let* ((all-keys-found t)
	     (style (car (org-cite-citation-style citation nil)))
	     (nocite (member style '("n" "nocite"))))
	(dolist (reference (org-cite-get-references citation))
	  (let ((boundaries (org-cite-key-boundaries reference)))
	    (add-face-text-property
	     (car boundaries) (cdr boundaries)
	     (let ((key (org-element-property :key reference)))
	       (if (or (org-cite-csl-activate--get-item key) (and nocite (string= key "*")))
		   'org-cite-key
		 (setq all-keys-found nil) 'error)))))
	(if (and all-keys-found (not nocite))
	    (progn
	      (put-text-property beg end 'cursor-sensor-functions
				 (list #'org-cite-csl-activate--sensor-fun))
	      (put-text-property (- end 1) end 'rear-nonsticky
				 '(cursor-sensor-functions)))
	  (put-text-property (- end 1) end 'rear-nonsticky nil)
	  (put-text-property beg end 'cursor-sensor-functions nil)
	  (put-text-property beg end 'display nil)
	  (put-text-property beg end 'help-echo nil))))))


;;; Register the activation processor
(org-cite-register-processor 'csl-activate 
  :activate #'org-cite-csl-activate)

(provide 'org-cite-csl-activate)
(provide 'oc-csl-activate)
;;; oc-csl-activate.el ends here
