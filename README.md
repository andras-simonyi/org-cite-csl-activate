# org-cite-csl-activate
An experimental CSL-based activation processor for Org citations.

## Requirements

+ Up-to-date `wip-cite-new' Org development branch.
+ The [citeproc-el](https://github.com/andras-simonyi/citeproc-el) library.

## Installation

Drop `oc-csl-activate.el` somewhere on your load path.

## Usage

The provided activation processor can be selected by executing

``` emacs-lisp
(require 'oc-csl-activate)
(setq org-cite-activate-processor 'csl-activate)
```



