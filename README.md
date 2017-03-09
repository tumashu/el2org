- [el2org 使用说明](#org2a38fa7)
  - [安装](#org8035b06)
    - [安装 el2org](#org6a62411)
    - [安装 ox-gfm](#orge515a44)
  - [配置](#org81aadf9)
  - [使用](#orge7f3764)


<a id="org2a38fa7"></a>

# el2org 使用说明

el2org 是一个简单的工具，它可以将 emacs-lisp 文件转换为 org 文件，前提 是 emacs-lisp 文件的格式要符合一些特定的要求。


<a id="org8035b06"></a>

## 安装


<a id="org6a62411"></a>

### 安装 el2org

1.  配置 melpa 源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET


<a id="orge515a44"></a>

### 安装 ox-gfm

el2org 可以使用 ox-gfm (Github Flavored Markdown exporter for Org Mode) 将 org 格式转换 为 github markdown 格式，但这个功能需要用户 **手动安装 ox-gfm**, 具体安装方式：

1.  配置 melpa 源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET ox-gfm RET

如果用户没有安装 ox-gfm, 那么，el2org 将使用 ox-md 后端生成 README.md。


<a id="org81aadf9"></a>

## 配置

    (require 'el2org)


<a id="orge7f3764"></a>

## 使用

1.  \`el2org-generate-readme' 可以从当前 elisp 文件的 Commentary 部份提取 相关内容，然后生成 README.md 文件。