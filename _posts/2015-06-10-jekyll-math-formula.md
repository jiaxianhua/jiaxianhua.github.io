---
layout: post
title: "Jekyll使用MathJax来显示数学式"
description: ""
category: latex
tags: [latex, jekyll]
---
{% include JB/setup %}

> 使用Jekyll写作文章的时候有可能需要内嵌一些数学公式, MathJax就是用来干这个的，试用了一下感觉非常方便。步骤如下:

## 修改html头部
---

在每个页面开头加上这么一句，在Jekyll下可以通过修改`default.html`**(_layouts/default.html)**加上。

{% highlight javascript %}
<script type="text/javascript"
	src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
{% endhighlight %}

## 本地安装kramdown
---

因为rdiscount和默认的markdown在解析带公式文件的时候都会出现一些问题，所以最简单办法还是安装kramdown。

`$ gem install kramdown --version 1.6.0`

`$ gem install kramdown`

## 修改_config.yml
---

把markdown选项修改为:

` markdown: kramdown`

## 输入公式
---

然后在发布的时候就可以使用$$来把需要显示的数学公式扩起来。像这样：

`$$ a^2 + b^2 = c^2 $$`

发布出来就是漂亮的公式了。

$$ a^2 + b^2 = c^2 $$
