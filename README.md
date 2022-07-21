# org-cite-csl-activate

![Citation rendering demo](demo.gif)

An experimental CSL-based activation processor for Org citations, which
fontifies citations using rendered previews generated by `citeproc-el`. In
addition, tooltips over citations show a ‘mini bibliography’ of the items
referred to.

**Table of Contents**

- [Requirements](#requirements)
- [Installation](#installation)
- [Setup](#setup)
- [Customization](#customization)
- [Known problems and limitations](#known-problems-and-limitations)
- [Acknowledgements](#acknowledgements)
- [License](#license)

## Requirements

+ Org 9.5 or later.
+ The [citeproc-el](https://github.com/andras-simonyi/citeproc-el) library.

## Installation

Drop `oc-csl-activate.el` somewhere on your load path.

## Setup

The library requires `cursor-sensor-mode` to be turned on in the Org buffer in
which citations should be rendered, and the provided activation processor should
be selected by executing

``` emacs-lisp
(require 'oc-csl-activate)
(setq org-cite-activate-processor 'csl-activate)
```

This causes _edited_ Org citations with valid cite keys to be rendered
immediately when the cursor leaves them, but unedited citations remain
unrendered. The interactive command

``` emacs-lisp
org-cite-csl-activate-render-all
```

in contrast, renders all Org citations in the current buffer.

One way of making sure that `cursor-sensor-mode` is turned on is adding the
corresponding command to `org-mode-hook`:

``` emacs-lisp
(add-hook 'org-mode-hook (lambda () (cursor-sensor-mode 1)))
```

The command `org-cite-csl-activate-render-all` can also be added to
`org-mode-hook` to render all citations upon opening an Org document but this
can slow down opening documents with a huge number of citations; see the section
[Known problems and limitations](#known-problems-and-limitations) for some
(anecdotal) details.

## Customization

### CSL style and locale

Citations and the associated ‘mini bibliographies’ are rendered in the default
CSL style and locale by default (typically, Chicago author-date and en_US), but this can be changed
by setting the variables `org-cite-csl-activate-use-document-style`and `org-cite-csl-activate-use-document-locale` variable to non-nil values:

``` emacs-lisp
(setq org-cite-csl-activate-use-document-style t)
(setq org-cite-csl-activate-use-document-locale t)
```

in which case the CSL style and locale (org `#+language:` keyword) set in the document is used.

> :warning: **Warning:** Setting the `org-cite-csl-activate-use-document-style` variable to non-nil when a CSL style which doesn’t belong to the `author-date` category is used will almost certainly cause rendering problems.

### Using the Citar cache for retrieving bibliographic entries

Users of [Citar](https://github.com/emacs-citar/citar) who want perfect
synchronization between the bibliographic data available for and presented by
Citar and what is accessed by org-cite-csl-activate can set the variable
`org-cite-csl-activate-use-citar-cache` to a non-nil value:

``` emacs-lisp
(setq org-cite-csl-activate-use-citar-cache t)
```

An important advantage of this setting is that Citar, in contrast to the default
item getter, automatically detects bibliography file changes and updates entry
information if necessary. On the negative side, Citar in certain cases stores
only a subset of the available metadata, so rendering might be a bit different
from exporting the document with the “csl” export processor.

## Known problems and limitations
This is mostly untested code, you will most probably encounter some problems and
glitches. In particular,

+ deleting citations when the cursor positioned _after_ the citation behaves
  weirdly, because the rendered citation is not removed and the user is unable
  to see what is happening under the replaced text until everything gets deleted;
+ citation rendering is not disambiguated (only within the same citation);
+ rendering a very large number of citations using
  `org-cite-csl-activate-render-all` may be slow, my (old) laptop renders
  approximately 400 citations per second on average. (The speed might be
  different with `org-cite-csl-activate-use-citar-cache` set to non-nil.)

## Acknowledgements

### Code contributions

- [Anders Johansson](https://github.com/andersjohansson)
- [TEC](https://github.com/tecosaur)

### Advice, discussion

- [Bruce D'Arcus](https://github.com/bdarcus)

## License

Copyright (C) 2021-2022 András Simonyi

Authors: András Simonyi

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program. If not, see http://www.gnu.org/licenses/.


