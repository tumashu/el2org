Note: this file is converted from el2org.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [What is el2org](#org24a344d)
    1.  [Installation](#org9b67cd2)
    2.  [Configure](#org91f7e3c)
    3.  [Usage](#org4767128)


<a id="org24a344d"></a>

# What is el2org

el2org is a simple tool, which can convert a emacs-lisp file to org file.
You can write code and document in a elisp file with its help.

               (convert to)                    (export to)
    elisp  -----------------> org (internal) --------------> other formats

Note: el2org.el file may be a good example.

![img](./snapshots/el2org.gif)


<a id="org9b67cd2"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET

    ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found,
    ox-md will be used as fallback.


<a id="org91f7e3c"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="org4767128"></a>

## Usage

1.  \`el2org-generate-file' can convert an elisp file to other file format
    which org's exporter support.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary"
    section.
3.  \`el2org-generate-html' can generate a html file from current elisp file
    and browse it.
4.  \`el2org-generate-org' can generate a org file from current elisp file.
