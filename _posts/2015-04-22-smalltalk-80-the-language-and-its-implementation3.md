---
layout: post
title: "Smalltalk-80: The Language and Its Implementation (3)"
description: ""
category: smalltalk 
tags: [smalltalk, book]
---
{% include JB/setup %}

## Subclasses
---


* subclass

	A class that inherits variables and methods from an existing class.

* superclass

	The class from which variables and methods are inherited.

* Object

	The class that is the root of the tree-structured class hier- archy.

* overriding

	Specifying a method in a subclass for the same message as a method in a superclass.

* super

	A pseudo-variable that refers to the receiver of a message; differs from self in where to start the search for methods.

* abstract class

	A class that specifies protocol, but is not able to fully im- plement it; by convention, instances are not created of this kind of class.

* subclassResponsibility

	A message to report the error that a subclass should have implemented one of the superclass's messages.

* shouldNotImplement

	A message to report the error that this is a message inherited from a superclass but explicitly not available to instances of the subclass.
