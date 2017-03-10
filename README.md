- [el2org 使用说明](#org7c334fa)
  - [安装](#orga704419)
    - [安装 el2org](#org22ab2e2)
    - [安装 ox-gfm](#orgf03a672)
  - [配置](#org6350779)
  - [使用](#org7042a81)


<a id="org7c334fa"></a>

# el2org 使用说明

el2org 是一个简单的工具，它可以将 elisp 文件转换为 org 文件，前提 是 elisp 文件的格式要符合一些特定的要求。


<a id="orga704419"></a>

## 安装


<a id="org22ab2e2"></a>

### 安装 el2org

1.  配置 melpa 源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET el2org RET


<a id="orgf03a672"></a>

### 安装 ox-gfm

el2org 可以使用 ox-gfm (Github Flavored Markdown exporter for Org Mode) 将 org 格式转换 为 github markdown 格式，但这个功能需要用户 **手动安装 ox-gfm**, 具体安装方式：

1.  配置 melpa 源，参考：<http://melpa.org/#/getting-started>
2.  M-x package-install RET ox-gfm RET

如果用户没有安装 ox-gfm, 那么，el2org 将使用 ox-md 后端生成 README.md。


<a id="org6350779"></a>

## 配置

    (require 'el2org)


<a id="org7042a81"></a>

## 使用

1.  \`el2org-generate-readme' 可以从当前 elisp 文件的 Commentary 部份提取 相关内容，然后生成 README.md 文件。