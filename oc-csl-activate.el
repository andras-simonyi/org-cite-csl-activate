;;; oc-csl-activate.el --- CSL-based citation activation  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 András Simonyi

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
(require 'oc)
(require 'oc-csl)


;;; Internal variables and functions

(defvar org-cite-csl-activate--processor-cache nil
  "Cache for the citation processor.")

(defun org-cite-csl-activate--processor ()
  "Return a `citeproc-el' processor for activation."
  (or org-cite-csl-activate--processor-cache
      (let* ((bibliography (org-cite-list-bibliography-files))
	     (processor
	      (citeproc-create
	       org-cite-csl--fallback-style-file
	       (org-cite-csl--itemgetter bibliography)
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

(defun org-cite-csl-activate--sensor-fun (_ prev motion)
  "Cursor sensor function for activated citations."
  (let* ((pos (if (eq motion 'left) prev (point)))
	 (element (org-with-point-at pos (org-element-context))))
    (when (memq (car element) '(citation citation-reference))
      (when (eq (car element) 'citation-reference)
	(setq element (org-element-property :parent element)))
      (pcase-let ((`(,beg . ,end) (org-cite-get-boundaries element)))
	(if (eq motion 'left)
	    (let ((proc (org-cite-csl-activate--processor)))
	      (citeproc-clear proc)
	      (let* ((info (list :cite-citeproc-processor proc))
		     (cit-struct (org-cite-csl--create-structure element info)))
		(citeproc-append-citations (list cit-struct) proc)
		(put-text-property beg end
				   'display
				   (car (citeproc-render-citations proc 'plain t)))
		(put-text-property beg end 'help-echo
				   (car (citeproc-render-bib proc 'plain nil)))))
	  (put-text-property beg end 'display nil))))))


;;; Utilities 
(defun org-cite-get-boundaries (citation)
  "Return the beginning and end position of CITATION.
Returns a (BEG . END) pair."
  (let ((beg (org-element-property :begin citation))
	(end (org-with-point-at (org-element-property :end citation)
	       (skip-chars-backward " \t")
	       (point))))
    (cons beg end)))


;;; Activation function 
(defun org-cite-csl-activate (citation)
  "Fontify CITATION object by rendering it with `citeproc-el'."
  ;; REVIEW: is this the optimal way to ensure that cursor-sensor-mode is on?
  (cursor-sensor-mode 1)
  (pcase-let ((`(,beg . ,end) (org-cite-get-boundaries citation)))
    (put-text-property beg end font-lock-multiline t)
    (add-face-text-property beg end 'org-cite)
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
	(put-text-property beg end 'display  nil)))))


;;; Register the activation processor
(org-cite-register-processor 'csl-activate 
  :activate #'org-cite-csl-activate)

(provide 'org-cite-csl-activate)
(provide 'oc-csl-activate)
;;; oc-csl-activate.el ends here
