---
layout: post
title: "simulator和emulator区别"
description: ""
category: game
tags: [diff, hardware, software, game]
---
{% include JB/setup %}

原文: <http://blog.csdn.net/laurensky/article/details/3323756>

## 解释一
---

**emulator**   
n.[计]仿真器。

通过软件方式，精确地在一种处理器上仿真另一种处理器或者硬件的运行方式。其目的是完全仿真被仿真硬件在接收到各种外界信息的时候的反应。我们现在常见的MAME、ePSXe等都是这一类。


**simulator**  
n.  模拟器。

通过某种手段，来模拟某些东西。不一定要完全正确的原理，追求的只是尽可能的相像。比如DWI、BandJAM等都属于这一类。
 
## 解释二
---
 模拟（Simulation）即选取一个物理的或抽象的系统的某些行为特征，用另一系统来表示它们的过程。
 模拟技术的高级阶段称为仿真模拟（Emulation）、系统仿真，即用一数据处理系统来全部或部分地模拟某一数据处理系统，以致于模仿的系统能想被模仿的系统一样接受同样的数据、执行同样的程序、获得同样的结果。

## 解释三
---

 **Emulation**：When one system performs in exactly the same way as another, though perhaps not at the same speed. A typical example would be emulation of one computer by ( a program running on) another. You migh use emulation as a replacement for a system whereas you would use a simulation if you just wanted to analyse it and make predictions about it.

 **Simulation**：Attempting to predict aspects of the behaviour of some system by creating an approximate (mathematical) model of it. This can be done by physical modelling, by writing a special-purpose computer program or using a more general simulation package, probably still aimed at a particular kind of simulation (e.g. structural engineering, fluid flow). Typical examples are aricraft flight simulators or electronic circuit simulators. A great many simulation languages exist, e.g. {Simula}.
  
## 解释四
---
  仿真器（Emulator），又称仿真程序，在软件工程中指可以使计算机或者其他多媒体平台（掌上电脑，手机）能够运行其他平台上的程序，常被错误的称为模拟器。仿真器多用于电视游戏和街机，也有一些用于掌上电脑。仿真器一般需要ROM才能执行，ROM的最初来源是一些原平台的ROM芯片，通过一些手段将原程序拷贝下来（这个过程一般称之为“dump”）然后利用仿真器加载这些ROM来实现仿真过程。  
  模拟器（simulator），又称模拟程序，在计算机科学技术的软件工程中，是指完全基于主机程序并模拟了目标处理器的功能和指令系统的程序。而仿真器（emulator）具有更强大的硬件模仿功能。
    还有很多很多的解释，相信看了上面的四个解释，对Simulator和Emulator的区别有感性的认识。

## 把握以下关键的几点，就可以很容易区别出Simulator和Emulator
---

1. Simulator中文叫模拟器；Emulator中文叫仿真器。
1. Simulator纯粹以软件来模拟源平台的功能和运行结果；Emulator以软件和硬件来模拟源平台的内部设计、行为和运行结果。

## 举例来说
---

1. 有使用硬件来模拟的，都是Emulator。比如基于单片机的模拟。（什么是叫使用硬件模拟？比如模拟源平台的Timer/PPU/SPU, 直接使用目标平台的Timer/PPU/SPU，那么就是硬件模拟）。
2. 一般的，在PC上运行的模拟器都叫Simulator，常见的是模拟LCD的显示画面; 在嵌入平台上运行的模拟器都是Emulator，因为在嵌入平台运行的话，为了提高效率，都会以对应的硬件模块来模拟源平台。
3. PC上的模拟器如果模拟其内部设计、行为，比如读取ROM文件，精确中断、异常，OS等都是Emulator。
