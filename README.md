- [What is el2org](#orge361888)
  - [Installation](#org5ed413c)
  - [Configure](#orgf3803c0)
  - [Usage](#orgcd7d7ab)


<a id="orge361888"></a>

# What is el2org

el2org is a simple tool，which can convert a emacs-lisp file to org file. You can write code and document in a elisp file with its help.

Note: el2org.el file may be a good example.


<a id="org5ed413c"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET

ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found, ox-md will be used as fallback.


<a id="orgf3803c0"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="orgcd7d7ab"></a>

## Usage

1.  \`el2org-orgify-if-necessary' can convert an elisp file to org-file.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary"

section.