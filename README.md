- [What is el2org](#org7213c99)
  - [Installation](#org9beb7bd)
  - [Configure](#org8e417a5)
  - [Usage](#orga2bc239)


<a id="org7213c99"></a>

# What is el2org

el2org is a simple tool，which can convert a emacs-lisp file to org file. You can write code and document in a elisp file with its help.


<a id="org9beb7bd"></a>

## Installation

1.  Config melpa source, please read: <http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET
3.  M-x package-install RET ox-gfm RET

    ox-gfm is needed by \`el2org-generate-readme', if ox-gfm can not be found, ox-md will be used as fallback.


<a id="org8e417a5"></a>

## Configure

    (require 'el2org)
    (require 'ox-gfm)


<a id="orga2bc239"></a>

## Usage

1.  \`el2org-orgify-if-necessary' can convert an elisp file to org-file.
2.  \`el2org-generate-readme' can generate README.md from elisp's "Commentary" section.