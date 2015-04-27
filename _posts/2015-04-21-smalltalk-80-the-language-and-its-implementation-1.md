---
layout: post
title: "Smalltalk 80: The Language and Its Implementation (1)"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

Title: Smalltalk-80: The Language and Its Implementation

Author: Adele Goldberg and David Robson

Link: <http://www.mirandabanda.org/bluebook/bluebook_imp_toc.html>

* System Class

> The Smalltalk-80 system includes a set of classes that provides the standard functionality of a programming language and environment: arithmetic, data structures, control structures, and input/output facilities. The functionality of these classes will be specified in detail in Part Two of this book. Figure 1.3 is a diagram of the system classes presented in Part Two. Lines are drawn around groups of related classes; the groups are labeled to indicate the chapters in which the specifications of the classes can be found.

* Arithmetic

> The Smalltalk-80 system includes objects representing both real and rational numbers. Real numbers can be represented with an accuracy of about six digits. Integers with absolute value less than 252428scan be represented exactly. Rational numbers can be represented using these integers. There are also classes for representing linear magnitudes (like dates and times) and random number generators.

* Data Structures
* Control Structures
* Programming Environment
* Viewing and Interacting
* Communication

|Terminology |Summary|
|---+---|
| object| A component of the Smalltalk-80 system represented by some private memory and a set of operations.|
| message| A request for an object to carry out one of its operations. The object to which a message is sent.|
| receiver " interface| The messages to which an object can respond.|
| class| A description of a group of similar objects.|
| instance| One of the objects described by a class.|
| instance variable| A part of an object's private memory.|
| method| A description of how to perform one of an object's operations.|
| primitive method| An operation performed ,directly by the Smalltalk-80 virtual machine.|
| FinancialHistory| The name of a class used as an example in this book.|
| system classes| The set of classes that come with the Smalltalk-80 system.|

# Expression Syntax
---

* Literals

1. Numbers
1. Characters
1. Strings 
1. Symbols 
1. Arrays

* Variables

1. Assignments
1. Pseudo-variable 
1. Names

* Messages

1. Selectors and Arguments 
1. Returning Values 
1. Parsing 
1. Formatting Conventions 
1. Cascading

* Blocks

1. Control Structures 
1. Conditionals 
1. Block Arguments

Summary of Terminology

1. Literals describe certain constant objects, such as numbers and character strings.
2. Variable names describe the accessible variables. The value of a variable name is the current value of the variable with that name.
3. Message expressions describe messages to receivers. The value of a message expression is determined by the method the message invokes. That method is found in the class of the receiver.
4. Block expressions describe objects representing deferred activities. Blocks are used to implement control structures.

语法简介

1. 文字描述某些常量对象,如数字和字符串。
1. 变量名称描述访问变量。一个变量的值的名字是变量的当前值与这个名字。
1. 消息表达式描述消息接收器。一条消息表达式的值是由方法调用的消息。方法是发现类上的接收器。
1. 块表达式描述对象代表延迟活动。块是用于实现控制结构。

|Terminology |Summary|
|---:+:---|
| **Literals** | 1. numbers <br />2. individual characters <br />3. strings of characters <br />4. symbols <br />5. arrays of other literal constants |
| **Numbers** | 1. 3 30.45 <br />2. -3 0.005<br />3. -14.0 13772<br /> 4. 1.586e-- 3<br />|
| **Strings** | 1. 'hi'<br /> 2. 'food'<br /> 3. 'the Smalltalk-80 system'<br />|

| **Symbols** | 1. #:bill<br />2.  #M63<br />|

| **Arrays** | 1. #(1 2 3)<br />2. #('food" 'utilities' 'rent" 'household' 'transportation' 'taxes' 'recreation')<br/>3. # (('one' 1) ('not" "negative') 0 -- 1)<br />4. #(9 'nine' $9 nine (0 "zero' $0()'e' $f 'g' $h 'i'))|

| **Variables** | 1. index<br /> 2. initiallndex <br /> 3. textEditor<br /> 4. bin14<br /> 5. bin14Totai <br /> 6. HouseholdFinances <br /> 7. Rectangle <br /> 8. IncomeReasons<br />|

| **Assignments** | 1. quantity <- 19<br /> 2.index <- initiallndex <br /> 3. index <- initialIndex <- 1|

| **Pseudo-variable Names** | 1. nil <br /> 2. true <br /> 3. false |

| **Messages** | 1. 3 + 4 : computes the sum of three and four. <br /> 2. index + 1 : adds one to the number named index.<br /> 3. index > limit : inquires whether or not the number named index is greater than the number named limit.<br /> 4. theta sin : computes the sine of the number named theta.<br /> 5. quantity sqrt : computes the positive square root of the number named quantity.<br />| 

| **Selectors and Arguments** | 1. Unary Message<br /> * theta sin<br /> * quantity sqrt <br /> * nameString size<br /> 2. Binary Message<br /> * 3 + 4<br /> * total - 1<br /> * total < = max<br /> 3. Keyword Messages<br /> * HouseholdFinances spend: 30.45 on: "food" <br /> * ages at: 'Brett Jorgensen' put: 3<br /> |

| **Returning Values** | 1. sum <- 3 + 4 <br /> 2. x <- theta sin <br /> index <- index + 1 <br /> 3. var <- HouseholdFinances spend: 32.50 on: 'utilities' <br /> |

| **Parsing** | 1. 1.5 tan rounded <br /> 2.  index + offset * 2 <br /> 3.  index + (offset. 2) <br /> 4. frame scale: factor max: 5 <br /> 5.  frame scale: (factor max: 5) <br /> |


> 1. Unary expressions parse left to right.
> 2. Binary expressions parse left to right.
> 3. Binary expressions take precedence over keyword expressions.
> 4. Unary expressions take precedence over binary expressions.
> 5. Parenthesized expressions take precedence over unary expressions.

> 1. 一元运算表达式解析从左到右。
> 2. 二元表达式解析从左到右。
> 3. 二元表达式优先于关键字表达式。
> 4. 一元运算表达式优先于二元表达式。
> 5. 带括号表达式优先于一元运算表达式。

| **Formatting Conventions** | A programmer is free to format expressions in various ways using spaces, tabs, and carriage returns. |

| **Cascading** | OrderedCollection new add: 1; add: 2; add: 3 <br /> Equals: <br /> temp ~ OrderedCoilection new. temp add: 1.<br /> temp add: 2.<br /> temp add: 3<br /> |

| **Blocks** | 1. [index <- index + 1]<br /> 2. [index <-index + 1. array at: index put: O]<br /> 3. [expenditures at: reason.]<br /> |

| **Control Structures** ||

1. Assign a block to incrementBIock. 
2. Assign a block to sumBIock.
3. Assign the number 0 to sum.
4. Assign the number 1 to index.
5. Sendthe message value to the block sumBIock. 
6. Send the message, 1 to the number 1.
7. Send the message + 1 to the number 0.
8. Assign the number 1 to sum.
9. Send the message value to the block IncrementBIock. 
10. Send the message + 1 to the number 1.
11. Assign the number 2 to index.
12. Send the message value to the block sumBIock.
13. Send the message * 2 to the number 2. 
14. Send the message + 4 to the number 1. 
15. Assign the number 5 to sum.

| **Conditionals** | (number \\\\ 2) = 0 <br /> ifTrue" [parity <- 0]<br /> ifFalse: [parity <- 1]<br /> |

| **Block Arguments** | 1. [ :array I total <- total + array size]<br /> 2. [:x:yl (x,x) + (y,y)]<br /> |

## Summary of Terminology
---

| Terminology |Summary |
|---+---| 
| **expression** | A sequence of characters that describes an object. | 
| **literal** | An expression describing a constant, such as a number or a string. | 
| **symbol** | A string whose sequence of characters is guaranteed to be different from that of any other symbol. | 
| **array** | A data structure whose elements are associated with integer indices. | 
| **variable name** | An expression describing the current value of a variable. | 
| **assignment** | An expression describing a change of a variable's value. | 
| **pseudo-variable name** | An expression similar to a variable name. However, unlike a variable name, the value of a pseudo-variable name cannot be changed by an assignment. | 
| **receiver** | The object to which a message is sent. |
| **message selector** | The name of the type of operation a message requests of its receiver. |
| **message argument** | An object that specifies additional information for an operation. |
| **unary message** | A message without arguments. |
| **keyword** | An identifier with a trailing colon. |
| **keyword message** | A message with one or more arguments whose selector is made up of one or more keywords. |
| **binary message** | A message with one argument whose selector is made up of one or two special characters. |
| **cascading** | A description of several messages to one object in a single expression. |
| **block** | A description of a deferred sequence of actions. |
| **block argument** | A parameter that must be supplied when certain blocks are evaluated. |
| **value** | A message to a block asking it to carry out the set of actions it represents. |
| **value:** | A keyword used in a message to a block that has block arguments; the corresponding message asks the block to carry out its set of actions. |
| **ifTrue:if False:** | Message to a Boolean requesting conditional selection. |
| **if False:if True:** | Message to a Boolean requesting conditional selection. |
| **ifTrue:** | Message to a Boolean requesting conditional selection. |
| **if False:** | Message to a Boolean requesting conditional selection. |
| **whileTrue:** | Message to a block requesting conditional repetition. |
