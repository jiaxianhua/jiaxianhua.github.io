---
layout: post
title: "synthesia"
description: ""
category: synthesia
tags: [synthesia, linux]
---
{% include JB/setup %}

## synthesia
---

<http://www.synthesiagame.com/>

<http://www.synthesiagame.com/download>

midi

<http://synthesia.ys168.com>

## install linux
---

VMware Fusion

ubuntu meta: <http://ubuntu-mate.org/vivid/>

> Mac (PowerPC) and IBM-PPC (POWER5) : NGr. not recognized.
> 
> ubuntu-mate-15.04-desktop-amd64.iso : OK.

## Install VMware Tools
---

1. Software & Update
1. Ubuntu Software
1. Download from : http://mirrors.aliyun.com/ubuntu
1. Close
1. Reload

{% highlight bash %}
$ sudo apt-get install build-essential
$ copy /media/${user}/VMWare\ Tools /tmp/
$ sudo tar xzvf *.tar.gz
$ cd vmware-tools-distrib
$ sudo ./vmware-install.pl
... Would you like to enable VMware automatic kernel modules?[no] yes
{% endhighlight %}

restart x or compulater

## install synthesia
---

{% highlight bash %}
$ mkdir github
$ cd !$
$ sudo apt-get install git
$ git clone https://github.com/linthesia/linthesia.git
$ cd linthesia
$ cat README
$ cat BUILD-DEPENDS

libgtkmm-2.4-dev
libgconfmm-2.6-dev
libgtkglextmm-x11-1.2-dev
libasound2-dev

$ sudo apt-get install `cat BUILD-DEPENDS`
$ sudo apt-get install autoconf
$ autoreconf -ivf
$ sudo apt-get install libtool
$ autoreconf -ivf

$ mkdir build
$ cd build     # Isolate compilation to speed future compilations
$ ../configure

$ make && sudo make install
{% endhighlight %}

## use linthesia

1. $ linthesia
1. select "*.mid" from "/use/local/share/linthesia/music/Popular/Gymnopedia.mid"
1. download *.mid, *.midi

## other
---

No voice ? e ?
