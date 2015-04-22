---
layout: post
title: "Smalltalk-80: The Language and Its Implementation (2)"
description: ""
category: smalltalk 
tags: [smalltalk, book]
---
{% include JB/setup %}


## Classes and Instances
---

| Terminology | Summary |
|---+---|
| **class** | An object that describes the implementation of a set of similar objects. |
| **instance** | One of the objects described by a class; it has memory and responds to messages. |
| **instance variable** | A variable available to a single object for the entire lifetime of the object; instance variables can be named or indexed. |
| **protocal description** | A description of a class in terms of its instances public message protocal. |
| **implementation description** | A message selector and a set of argument names, one for each argument that a message with this selector must have. |
| **temporary variable** | A variable created for a specific activity and available only for the duration of that activity. |
| **class variable** | A variable shared by all the instances of a single class. |
| **global variable** | A variable shared by all the instances of all classes. |
| **pool variable** | A variable shared by the instances of a set of classes. |
| **Smalltalk** | A pool shared by all classes that contains the global variables. |
| **method** | A procedure describing how to perform one of an object's operations; it is made up of a message pattern, temporary variable declaration, and a sequence of expressions. A method is executed when a message matching its message pattern is sent to an instance of the class in which the method is found. |
| **argument name** | Name of a pseudo-variable available to a method only for the duration of that method's execution; the value of the argument names are the arguments of the message that invoked the method. |
| **up** | When used in a method, indicates that the value of the next expression is to be the value of the method. |
| **self** | A pseudo-variable referring to the receiver of a message. |
| **message category** | A group of methods in a class description. |
| **primitive method** | An operation performed directly by the Smalltalk-80 virtual machine; it is not described as a sequence of Smalltalk-80 expressions. |

