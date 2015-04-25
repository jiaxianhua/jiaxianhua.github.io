---
layout: post
title: "Pharo by Example 4"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## Understanding message syntax
---

### Identifying messages
---

* A message is composed of the message *selector* and the optional message arguments.
* A message is sent to a receiver.
* The combination of a message and its receiver is called a message send
as shown in Figure 4.1.

![Figure 4.1](/assets/img/smalltalk/Figure 4.1.png)

Figure 4.1: Two messages composed of a receiver, a method selector, and a set of arguments.

> A message is always sent to a receiver, which can be a single literal, a block or a variable or the result of evaluating another message.

## Three kinds of messages
---

* **Unary messages** are messages that are sent to an object without any other information. For example in **3 factorial**, *factorial* is a unary message.

> Unary messages are messages that do not require any argument.
> They follow the syntactic template: receiver selector

* **Binary messages** are messages consisting of operators (often arithmetic). They are binary because they always involve only two objects: the receiver and the argument object. For example in 10 + 20, + is a binary message sent to the receiver 10 with argument 20.

> Binary messages are messages that require exactly one argument and whose selector is composed of a sequence of characters from: +, −, \*, /, &, =, >, |, <, ≥, and @. −− is not possible.
> They follow the syntactic template: receiver selector argument.

* **Keyword** messages are message sconsisting of one or more keywords,each ending with a colon (:) and taking an argument. For example in anArray at: 1 put: 10, the keyword at: takes the argument 1 and the keyword put: takes the argument 10.

> Keyword based messages are messages that require one or more arguments. Their selector consists of one or more keywords each ending in a colon (:). They follow the syntactic template:
> receiver selectorWordOne: argumentOne wordTwo: argumentTwo.

### Message composition
---

1. Unary messages are always sent first, then binary messages and finally keyword messages.
2. Messages in parentheses are sent prior to any kind of messages.
3. Messages of the same kind are evaluated from left to right.

#### Unary > Binary > Keywords
---

> **Rule One.** Unary messages are sent first, then binary messages, and finally keyword based messages.
>
> **Unary > Binary > Keyword**

#### Parentheses first
---

> **Rule Two.** Parenthesised messages are sent prior to other messages.
>
> **(Msg) > Unary > Binary > Keyword**

#### From left to right
---

> **Rule Three.** When the messages are of the same kind, the order of evaluation is from left to right.

#### Arithmetic inconsistencies
---

> **In Smalltalk,** arithmetic operators such as + and * do not have different priority. + and * are just binary messages, therefore * does not have priority over +. Use parentheses to obtain the desired result. 

### Hints for identifying keyword messages
---

#### Parentheses or not?
---

> The characters [, ], ( and ) delimit distinct areas. Within such an area, a keyword message is the longest sequence of words terminated by : that is not cut by the characters ., or ;. When the characters [, ], ( and ) surround some words with colons, these words participate in the keyword message local to the area defined.

#### When to use [ ] or ( )
---
You may also have problems understanding when to use square brackets rather than parentheses. The basic principle is that you should use [ ] when you do not know how many times, potentially zero, an expression should be evaluated. [expression] will create a block closure (i.e., an object) from expression, which may be evaluated any number of times (possibly zero), depending on the context. Here note that an expression can either be a message send, a variable, a literal, an assignment or a block.

Hence the conditional branches of ifTrue: or ifTrue:ifFalse: require blocks. Following the same principle both the receiver and the argument of a whileTrue: message require the use of square brackets since we do not know how many times either the receiver or the argument should be evaluated.

Parentheses, on the other hand, only affect the order of sending messages.
So in (expression), the expression will always be evaluated exactly once.

> |---+---|
> |[ x isReady ] whileTrue: [ y doSomething ] | *"both the receiver and the argument must be blocks"* |
> | 4 timesRepeat: [ Beeper beep ] | *"the argument is evaluated more than once, so must be a block"* |
> | (x isReady) ifTrue: [ y doSomething ] | *"receiver is evaluated once, so is not a block"* |

### Expression sequences
---

`句号是分隔符，不是中止符.`

Expressions (i.e., messages sends, assignments. . . ) separated by periods are evaluated in sequence. Note that there is no period between a variable definition and the following expression. The value of a sequence is the value of the last expression. The values returned by all the expressions except the last one are ignored. Note that the period is a separator and not a terminator. Therefore a final period is optional.

### Cascaded messages
---

`级联消息`

Smalltalk offers a way to send multiple messages to the same receiver using a semicolon (;). This is called the cascade in Smalltalk jargon.

> Expression Msg1 ; Msg2

{% highlight smalltalk linenos %}
Transcript show: 'Pharo is '.
Transcript show: 'fun '.
Transcript cr.
{% endhighlight %}

is equivalent to:

{% highlight smalltalk linenos %}
Transcript
    show: 'Pharo is';
    show: 'fun ';
    cr
{% endhighlight %}


## Chapter summary
---
 * message is always sent to an object named the *receiver* which may be the result of other message sends.
 * **Unary messages** are messages that do not require any argument. They are of the form of receiver **selector**.
 * **Binary messages** are messages that involve two objects, the receiver and another object and whose selector is composed of one or more characters from the following list: +, −, \*, /, \|, &, =, >, <,  ̃, and @. They are of the form: receiver **selector** argument
 * **Keyword messages** are messages that involve more than one object and that contain at least one colon character (:).
 They are of the form: receiver **selectorWordOne:** argumentOne **wordTwo:** argumentTwo
 * **Rule One.** Unary messages are sent first, then binary messages, and finally keyword messages.
 * **Rule Two.** Messages in parentheses are sent before any others.
 * **Rule Three.** When the messages are of the same kind, the order of evaluation is from left to right.
 * In Smalltalk, traditional arithmetic operators such as + and * have the same priority. + and * are just binary messages, therefore * does not have priority over +. You must use parentheses to obtain a different result.
