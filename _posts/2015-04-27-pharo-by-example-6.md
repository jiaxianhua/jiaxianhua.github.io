---
layout: post
title: "The Pharo programming environment"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

### Overview
---

* The **Browser** is the central development tool. You will use it to create, define, and organize your classes and methods. Using it you can also navigate through all the library classes: unlike other environments where the source code is stored in separate files, in Smalltalk all classes and methods are contained in the image.
* The **Message Names** tool is used to look at all of the methods with a particular selector, or with a selector containing a substring.
* The **Method Finder** tool will also let you find methods, but according to what they do as well as what they are called.
* The **Monticello Browser** is the starting point for loading code from, and saving code in, Monticello packages.
* The **Process Browser** provides a view on all of the processes (threads) executing in Smalltalk.
* The **Test Runner** lets you run and debug SUnit tests, and is described in Chapter 7.
* The **Transcript** is a window on the Transcript output stream, which is useful for writing log messages and has already been described in Section 1.5.
* The **Workspace** is a window into which you can type input. It can be used for any purpose, but is most often used for typing Smalltalk expressions and executing them as do it s. The use of the workspace was also illustrated in Section 1.5.

## Chapter summary
---

* The standard browser is your main interface for browsing existing cate- gories, classes, method protocols and methods, and for defining new ones. The browser offers several useful buttons to directly jump to senders or implementors of a message, versions of a method, and so on.
* There exist several different browsers (such as the OmniBrowser and the Refactoring Browser), and several specialized browsers (such as the hierarchy browser) which provide different views of classes and methods.
* From any of the tools,you can highlight the name of a class or a method and immediately jump to a browser by using the keyboard shortcut CMD–b.
* You can also browse the Smalltalk system programmatically by sending messages to SystemNavigation default.
* Monticello is a tool for exporting, importing, versioning and sharing packages of classes and methods. A Monticello package consists of a category, subcategories, and related methods protocols in other cate- gories.
* The inspector and the explorer are two tools that are useful for exploring and interacting with live objects in your image. You can even inspect tools by meta-clicking to bring up their morphic halo and selecting the debug handle .
* The debugger is a tool that not only lets you inspect the run-time stack of your program when an error is raised, but it also enables you to interact with all of the objects of your application, including the source code. In many cases you can modify your source code from the debugger and continue executing. The debugger is especially effective as a tool to support test-first development in tandem with SUnit (Chapter 7).
* The process browser lets you monitor, query and interact with the pro- cesses current running in your image.
* The method finder and them essage names browser are two tools for locating methods. The first is more useful when you are not sure of the name, but you know the expected behaviour. The second offers a more advanced browsing interface when you know at least a fragment of the name.
* Change sets are automatically generated logs of all changes to the source code of your image. They have largely been superseded by Monticello as a means to store and exchange versions of your source code, but are still useful, especially for recovering from catastrophic failures, however rare these may be.
* The file list browser is a tool for browsing the file system. It also allows you to filein source code from the file system.
* In case your image crashes before you could save it or backup your source code with Monticello, you can always recover your most recent changes using a change list browser. You can then select the changes you want to replay and file them into the most recent copy of your image.
