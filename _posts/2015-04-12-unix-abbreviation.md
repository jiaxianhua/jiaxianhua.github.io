---
layout: post
title: "unix 缩写风格"
description: ""
category: Linux
tags: [Linux, command]
---
{% include JB/setup %}

原文： <http://i.linuxtoy.org/docs/guide/ch02s02.html>

# 缩写习惯
---

构建于图表界面之上的操作系统，使用鼠标作为主输入设备，是否使用缩写并不重要。比如Windows系统中的目录，几乎都是全称。。。点击两次鼠标进入文件夹pf，并不意味着点击13次才能进入文件夹Program Files

而构建于命令行之上的操作系统，如Linux，只要3个字母以上的单词，几乎都要缩写。例如:cd命令是Change Directory的缩写。作为常用命令，如果使用它的全称Change Directory绝对是无聊和乏味的工作。

## 最常见的缩写，取每个单词的首字母，如

**cd** Change Directory  
**dd** Disk Dump  
**df** Disk Free  
**du** Disk Usage   
**pwd** Print Working Directory   
**ps** Processes Status   
**PS** Prompt Strings   
**su** Substitute User   
**rc** Run Command   
**Tcl** Tool Command Language   
**cups** Common Unix Printing System   
**apt** Advanced Packageing Tool   
**bg** BackGround   
**ping** Packet InterNet Grouper  

## 如果首字母后为**h**，通常保留

**chsh** CHange SHell  
**chmod** CHange MODe  
**chown** CHange OWNer  
**chgrp** CHange GRoup  
**bash** Bourne Again SHell  
**zsh** Z SHell  
**ksh** Korn SHell 
**ssh** Secure SHell 

## 递归缩写也属于这一类，如：

**GNU** GNU's Not Unix  
**PHP** PHP: Hypertext Preprocessor  
**RPM** RPM Package Manager  
**WINE** WINE Is Not an Emulator  
**PNG** PNG's Not Gif  
**nano** Nano's ANOther editor  

** 有些缩写可能有多种定义，如：

> **rpm**
>
> RPM Package Manager
>
> RetHad Package Manager
>
> **bc**
>
> Basic Calculator
>
> Better Calculator
>

## 这方面Emacs可谓独领风骚：

	Emacs May Allow Customized Screwups
	Emacs Manuals Are Cryptic and Surreal
	Eventually Munches All Computer Storage
	Eight Megabytes And Constantly Swapping
	Elsewhere Maybe All Commands are Simple
	Excellent Manuals Are Clearly Suppressed
	Emacs May Alienate Clients and Supporters
	Except by Middle Aged Computer Scientists
	Extended Macros Are Considered Superfluous
	Every Mode Accelerates Creation of Software
	Each Manual's Audience is Completely Stupefied
	Exceptionally Mediocre Algorithm for Computer Scientists
	Easily Maintained with the Assistance of Chemical Solutions
	Eradication of Memory Accomplished with Complete Simplicity  

## 如果只有一个单词，通常取每个章节的首字母：

**cp** CoPy  
**ln** LiNk  
**ls** LiSt  
**mv** MoVe  
**rm** ReMove  

## 对于目录，通常使用前几个字母作为缩写：

**bin** BINaries  
**dev** DEVices  
**etc** ETCetera  
**lib** LIBrary  
**var** VARiable  
**proc** PROCesses  
**sbin** Superuser BINaries  
**tmp** TeMPorary  
**usr** Unix Shared Resources  

## 这种缩写的其它情况

**diff** DIFFerences  
**cal** CALendar  
**cat** CATenate  
**ed** EDitor  
**exec** EXECute  
**tab** TABle  
**regexp** REGular EXPression  

## 如果某种缩写比较深入人心，例如**mesg**代表**message**，在新的复合缩写中，将沿用这种缩写方式

**dmesg** Disgnostic MESsaGe  
**sed** Stream EDitor  
**stty** Set TTY  
**fstab** FileSystem TABle  
**passwd** PASSWorD  

## 有些缩写中，第一个字母**g**，代表**GNU**

**awk** Aho Weiberger and Kernighan  
**gawk** GNU AWK  
**gpg** GNU Privacy Guard  
**grep** GNU Regular Expression Print  
**egrep** Extended GREP  

---

定义中包含自身缩写，如 GNU：

> GNU's Not Unix

使用这个定义来解释定义中的缩写：

> (GNU's Not Unix)'s Not Unix

这意味着它是可以无限递归的：

> (((((GNU's Not Unix)'s Not Unix)'s Not Unix)'s Not Unix)'s Not Unix)'s Not Unix ……
