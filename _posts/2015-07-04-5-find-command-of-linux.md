---
layout: post
title: "Linux的五个查找命令：find,locate,whereis,which,type"
description: ""
category: Linux
tags: [Linux]
---
{% include JB/setup %}

## 原文: <http://www.ruanyifeng.com/blog/2009/10/5_ways_to_search_for_files_using_the_terminal.html>
---

最近，我在学习Linux，下面是一些笔记。

使用电脑的时候，经常需要查找文件。

在Linux中，有很多方法可以做到这一点。国外网站LinuxHaxor总结了五条命令，你可以看看自己知道几条。大多数程序员，可能经常使用其中的2到3条，对这5条命令都很熟悉的人应该是不多的。

![find-1](/assets/img/linux/find-1.png)

## find
---

find是最常见和最强大的查找命令，你可以用它找到任何你想找的文件。

find的使用格式如下：

{% highlight bash %}
　　$ find <指定目录> <指定条件> <指定动作>

　　- <指定目录>： 所要搜索的目录及其所有子目录。默认为当前目录。

　　- <指定条件>： 所要搜索的文件的特征。

　　- <指定动作>： 对搜索结果进行特定的处理。
{% endhighlight %}

如果什么参数也不加，find默认搜索当前目录及其子目录，并且不过滤任何结果（也就是返回所有文件），将它们全都显示在屏幕上。

find的使用实例：

{% highlight bash %}
　　$ find . -name "my*"
{% endhighlight %}

搜索当前目录（含子目录，以下同）中，所有文件名以my开头的文件。

{% highlight bash %}
　　$ find . -name "my*" -ls
{% endhighlight %}

搜索当前目录中，所有文件名以my开头的文件，并显示它们的详细信息。

{% highlight bash %}
　　$ find . -type f -mmin -10
{% endhighlight %}

搜索当前目录中，所有过去10分钟中更新过的普通文件。如果不加-type f参数，则搜索普通文件+特殊文件+目录。

![find-2](/assets/img/linux/find-2.png)

## locate
---

locate命令其实是“find -name”的另一种写法，但是要比后者快得多，原因在于它不搜索具体目录，而是搜索一个数据库（/var/lib/locatedb），这个数据库中含有本地所有文件信息。Linux系统自动创建这个数据库，并且每天自动更新一次，所以使用locate命令查不到最新变动过的文件。为了避免这种情况，可以在使用locate之前，先使用updatedb命令，手动更新数据库。

locate命令的使用实例：

{% highlight bash %}
　　$ locate /etc/sh
{% endhighlight %}

搜索etc目录下所有以sh开头的文件。

{% highlight bash %}
　　$ locate ~/m
{% endhighlight %}

搜索用户主目录下，所有以m开头的文件。

{% highlight bash %}
　　$ locate -i ~/m
{% endhighlight %}

搜索用户主目录下，所有以m开头的文件，并且忽略大小写。

![find-3](/assets/img/linux/find-3.png)

## whereis
---

whereis命令只能用于程序名的搜索，而且只搜索二进制文件（参数-b）、man说明文件（参数-m）和源代码文件（参数-s）。如果省略参数，则返回所有信息。

whereis命令的使用实例：

{% highlight bash %}
　　$ whereis grep
{% endhighlight %}

![find-4](/assets/img/linux/find-4.png)

## which
---

which命令的作用是，在PATH变量指定的路径中，搜索某个系统命令的位置，并且返回第一个搜索结果。也就是说，使用which命令，就可以看到某个系统命令是否存在，以及执行的到底是哪一个位置的命令。

which命令的使用实例：

{% highlight bash %}
　　$ which grep
{% endhighlight %}

![find-5](/assets/img/linux/find-5.png)

## type
---

type命令其实不能算查找命令，它是用来区分某个命令到底是由shell自带的，还是由shell外部的独立二进制文件提供的。如果一个命令是外部命令，那么使用-p参数，会显示该命令的路径，相当于which命令。

type命令的使用实例：

{% highlight bash %}
　　$ type cd
{% endhighlight %}

系统会提示，cd是shell的自带命令（build-in）。

{% highlight bash %}
　　$ type grep
{% endhighlight %}

系统会提示，grep是一个外部命令，并显示该命令的路径。

{% highlight bash %}
　　$ type -p grep
{% endhighlight %}

加上-p参数后，就相当于which命令。

![find-6](/assets/img/linux/find-6.png)
