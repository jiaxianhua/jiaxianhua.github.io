---
layout: post
title: "个人常用软件及配置"
description: ""
category: 
tags: [software]
---
{% include JB/setup %}

OS X Software

## Xcode 
---

{% highlight bash %}
$ xcode-select --print-path
$ sudo xcode-select -switch /Developer/Applications/Xcode-Beta.app
$ xcode-select --print-path
{% endhighlight %}

## iterm2

<http://www.iterm2.com>

## on-my-zsh
---

<http://ohmyz.sh>

{% highlight bash %}
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
{% endhighlight %}

## Homebrew
---

<http://brew.sh>

Install Homebrew

{% highlight bash %}
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
{% endhighlight %}

## ruby version manager
---

<http://www.rvm.io>

{% highlight bash %}
$curl -sSL https://get.rvm.io | bash -s stable
{% endhighlight %}

## 替换系统 vim

{% highlight bash %}
$ brew install vim
$ vim .zshrc

$PATH=/usr/local/Cellar/vim/7.4.826/bin:$PATH
{% endhighlight %}

## vim 配置
---

github地址：<https://github.com/spf13/spf13-vim>

## 其它

![1](/assets/img/ios/software/1.png)

![2](/assets/img/ios/software/2.png)
