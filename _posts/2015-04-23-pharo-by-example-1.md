---
layout: post
title: "Pharo by Example 1"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}


Site:  <http://pharobyexample.org/versions/PBE1-2009-10-28.pdf>

Title: Pharo by Example

Author: Andrew P. Black Stéphane Ducasse Oscar Nierstrasz Damien Pollet

## A quick tour of Pharo
---

This chapter has introduced you to the Pharo environment and shown you how to use some of the major tools, such as the browser, the method finder, and the test runner. You have also seen a little of Pharo’s syntax, even though you may not understand it all yet.

• A running Pharo system consists of a virtual machine, a sources file, and image and changes files. Only these last two change, as they record a snapshot of the running system.

• When you restore a Pharo image, you will find yourself in exactly the same state — with the same running objects — that you had when you last saved that image.

• Pharo is designed to work with a three-button mouse to click, action- click or meta-click. If you don’t have a three-button mouse, you can use modifier keys to obtain the same effect.

• You click on the Pharo background to bring up the World menu and launch various tools.

• A workspace is a tool for writing and evaluating snippets of code. You can also use it to store arbitrary text.

• You can use keyboard shortcuts on text in the workspace, or any other tool, to evaluate code. The most important of these are do it
**(CMD–d)**, print it **(CMD–p)**, inspect it **(CMD–i)**, explore it **(CMD–I)** and browse it **(CMD–b)**.

• ThebrowseristhemaintoolforbrowsingPharocode,andfordeveloping new code.

• The test runner is a tool for running unit tests. It also supports Test Driven Development.
