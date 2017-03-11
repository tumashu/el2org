- [What is el2org](#org6260a57)
  - [Installation](#org2a1efc9)
  - [Configure](#org3f25817)
  - [Usage](#orgb0131e7)


<a id="org6260a57"></a>

# What is el2org

el2org is a simple tool，which can convert a emacs-lisp file to org file. You can write code and document in a elisp file with its help.

Note: el2org.el file may be a good example.


<a id="org2a1efc9"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET

    ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found, ox-md will be used as fallback.


<a id="org3f25817"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="orgb0131e7"></a>

## Usage

1.  \`el2org-generate-file' can convert an elisp file to other file format which org's exporter support.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary" section.
3.  \`el2org-generate-html' can generate a html file from current elisp file and browse it.