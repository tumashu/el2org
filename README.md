- [What is el2org](#orgd7bde06)
  - [Installation](#orgd6f3d5c)
  - [Configure](#org1a20421)
  - [Usage](#org7c26faf)


<a id="orgd7bde06"></a>

# What is el2org

el2org is a simple tool，which can convert a emacs-lisp file to org file. You can write code and document in a elisp file with its help.

Note: el2org.el file may be a good example.


<a id="orgd6f3d5c"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET

    ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found, ox-md will be used as fallback.


<a id="org1a20421"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="org7c26faf"></a>

## Usage

1.  \`el2org-orgify-if-necessary' can convert an elisp file to org-file.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary" section.