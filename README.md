Note: this file is auto converted from el2org.el by [el2org](https://github.com/tumashu/el2org), please do not edit it by hand!!!


# &#30446;&#24405;

1.  [What is el2org](#orgfbaf885)
    1.  [Installation](#orgf6eded7)
    2.  [Configure](#org27e2b16)
    3.  [Usage](#org1242b25)


<a id="orgfbaf885"></a>

# What is el2org

el2org is a simple tool, which can convert a emacs-lisp file to org file.
You can write code and document in a elisp file with its help.

               (convert to)                    (export to)
    elisp  -----------------> org (internal) --------------> other formats

Note: el2org.el file may be a good example.

![img](./snapshots/el2org.gif)


<a id="orgf6eded7"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET
    
    ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found,
    ox-md will be used as fallback.


<a id="org27e2b16"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="org1242b25"></a>

## Usage

1.  \`el2org-generate-file' can convert an elisp file to other file format
    which org's exporter support.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary"
    section.
3.  \`el2org-generate-html' can generate a html file from current elisp file
    and browse it.
4.  \`el2org-generate-org' can generate a org file from current elisp file.

