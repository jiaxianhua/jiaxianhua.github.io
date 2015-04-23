---
layout: post
title: "Pharo by Example 2"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## A first application
---

* Creating a new Package
    1. open Pharo 4.
    1. System Browser.
    1. Add Package... Input *PBE-LightsOut*.

* Defining the class LOCell
    1.  On Categories and Packages

        A category is simply a collection of related classes in a Smalltalk image.
        A package is a collection of related classes and extension methods that may be versioned using the Monticello versioning tool.

    1. Creating a new class

        * Replace Object by SimpleSwitchMorph.
        * Replace NameOfSubClass by LOCell.
        * Add mouseAction to the list of instance variables.
        * To actually send this message, you must **accept** *(CMD+s)* it.

        Defining the class LOCell

{% highlight smalltalk linenos %}
SimpleSwitchMorph subclass: #LOCell
    instanceVariableNames: 'mouseAction'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'PBE−LightsOut'
{% endhighlight %}

* Adding methods to a class
    1.  *Select the protocol no messages (--all--) in the protocol pane.*
        * Initializing instances of LOCell
        * input *initiali* Return. then input other code.
        * **Accept** *(CMD+s)* this method definition.

{% highlight smalltalk linenos %}
initialize
    super initialize.
    self label: ''.
    self borderWidth: 2.
    bounds := 0@0 corner: 16@16. 
    offColor := Color paleYellow.
    onColor := Color paleBlue darker. 
    self useSquareCorners.
    self turnOff
{% endhighlight %}

* Inspecting an object
    1. Open a *playground* (workspace). Type the expression LOCell new and **inspect it** .
    1. Select root of *self*.
    1. in the down windows, after **self** input.
    1. **Accept** *(CMD+d)*.

{% highlight smalltalk linenos %}
"a LOCell(937951232)"
self
self bounds: (200@200 corner: 250@250).
self openInWorld.
{% endhighlight %}

* Defining the class LOGame
    1.  Now let’s create the other class that we need for the game, which we will call LOGame.
        Make the class definition template visible in the browser main window.
        Defining the LOGame class

{% highlight smalltalk linenos %}
initialize
    | sampleCell width height n |
    super initialize.
    n := self cellsPerSide.
    sampleCell := LOCell new.
    width := sampleCell width.
    height := sampleCell height.
    self bounds: (5@5 extent: ((width*n) @(height*n)) + (2 * self borderWidth)).
    cells := Matrix new: n  tabulate: [ :i :j | self newCellAt: i at: j ].
{% endhighlight %}

* Organizing methods into protocols

## End of statement list encountered ->
---

> forget lastline "."

## Chapter summary
---

* Categories are groups of related classes.
* A new class is created by sending a message to its superclass.
* Protocols are groups of related methods.
* A new method is created or modified by editing its definition in the browser and then accepting the changes.
* The inspector offers a simple, general-purpose GUI for inspecting and interacting with arbitrary objects.
* The browser detects usage of undeclared methods and variables, and offers possible corrections.
* The initialize method is automatically executed after an object is created in Pharo. You can put any initialization code there.
* The debugger provides a high-level GUI to inspect and modify the state of a running program.
* You can share source code filing out a category.
* A better way to share code is to use Monticello to manage an external repository, for example defined as a SqueakSource project.
