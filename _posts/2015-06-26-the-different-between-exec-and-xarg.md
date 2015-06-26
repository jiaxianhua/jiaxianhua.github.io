---
layout: post
title: "exec与xargs区别"
description: ""
category: linux
tags: [linux]
---
{% include JB/setup %}

原文：<http://blog.chinaunix.net/uid-20662363-id-1904149.html>
---

exec:

> 对符合条件的文件执行所给的Linux 命令，而不询问用户是否需要执行该命令。{}表示命令的参数即为所找到的文件,以；表示comman命令的结束。\是转义符，因为分号在命令中还有它用途，所以就用一个\来限定表示这是一个分号而不是表示其它意思。

-ok：

> 和-exec的作用相同，格式也一样，只不过以一种更为安全的模式来执行该参数所给出的shell给出的这个命令之前，都会给出提示，让用户来确定是否执行。

xargs:

> 要结合管道来完成
> 
> 格式：`find [option] express |xargs command`

## 我们看看`exec`和`xargs`都是如何传参数的。
---

### 首先看看`exec`：
---

{% highlight bash %}
$ ls
index.skin1 skin1
$ find -type f -exec echo file {} \;
file ./skin1
file ./index.skin1
{% endhighlight %}

很明显，`exec`是对每个找到的文件执行一次命令，除非这单个的文件名超过了几k，否则不会出现命令行超长出错的问题。

### 我们再看看xargs:
---

{% highlight bash %}
$ ls
index.skin1 skin1
$ find -type f | xargs echo
find -type f | xargs echo
./skin1 ./index.skin1
{% endhighlight %}

这里大家看到，`xargs`是把所有找到的文件名一股脑的转给命令。当文件很多时，这些文件名组合成的命令行参数很容易超长，导致命令出错。

同时，这也是 `find | xargs` 这种组合在处理有空格字符的文件名时之所以出错的原因。

这时执行的命令已经不知道这些空格那些是分割符、那些是文件名中所包含的！而用`exec`则不会有这两个问题。

下面是一个演示：

{% highlight bash %}
$ mkdir TEST
$ cd TEST
/home/xxx/TEST
$ touch "file a"
$ touch "file b"
$ ls
file a file b
$ find -type f | xargs rm
rm: 无法删除‘./file’: 没有那个文件或目录
rm: 无法删除‘a’: 没有那个文件或目录
rm: 无法删除‘./file’: 没有那个文件或目录
rm: 无法删除‘b’: 没有那个文件或目录
123$ ls
file a file b
$ find -type f -exec rm {} \;
$ ls
$
{% endhighlight %}

从这里可以看出`exec`的缺点是:

* 每处理一个文件/目录，都要启动一次命令，效率不好; 
* 格式麻烦，必须用 {} 做文件的代位符，必须用 \; 
* 作为命令的结束符，书写不便。

* 而xargs不能操作文件名有空格的文件。

所以如果要使用的命令支持一次处理多个文件，并且也知道这些文件里没有带空格的、文件数目也不大，那么使用 `xargs`比较方便; 否则，就要用 `exec` 了。

