---
layout: post
title: "Pharo by Example 7"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## SUnit
---

### Why testing is important
---

Tests play several roles.

* **First**, they provide documentation of the functionality that they cover. 
**Moreover**, the documentation is active: watching the tests pass tells you that the documentation is up-to-date. 
* **Second**, tests help developers to confirm that some changes that they have just made to a package have not broken anything else in the system — and to find the parts that break when that confidence turns out to be misplaced.
* **Finally**, writing tests at the same time as — or even before — programming
forces you to think about the functionality that you want to design, and how it should appear to the client, rather than about how to implement it.

### What makes a good test?
---

Writing good tests is a skill that can be learned most easily by practicing. Let us look at the properties that tests should have to get a maximum benefit.

1. Tests should be repeatable. You should be able to run a test as often as you want, and always get the same answer.
2. Tests should run without human intervention. You should even be able to run them during the night.
3. Tests should tell a story. Each test should cover one aspect of a piece of code. A test should act as a scenario that you or someone else can read to understand a piece of functionality.
4. Tests should have a change frequency lower than that of the functionality they cover: you do not want to have to change all your tests every time you modify your application. One way to achieve this is to write tests based on the public interfaces of the class that you are testing. It is OK to write a test for a private “helper” method if you feel that the method is complicated enough to need the test, but you should be aware that such a test may have to be changed, or thrown away
   entirely, when you think of a better implementation.

### SUnit by example
---

#### Step 1: create the test class

An Example Set Test class
{% highlight smalltalk linenos %}
TestCase subclass: #ExampleSetTest
    instanceVariableNames: 'full empty'
    classVariableNames: '' poolDictionaries: ''
    category: 'MySetTest'
{% endhighlight %}

#### Step 2: initialize the test context

{% highlight smalltalk linenos %}
ExampleSetTest >>
setUp
    empty := Set new.
    full := Set with: 5 with: 6
{% endhighlight %}

#### Step 3: write some test methods

{% highlight smalltalk linenos %}
ExampleSetTest >>
testIncludes
    self assert: (full includes: 5).
    self assert: (full includes: 6)
{% endhighlight %}

{% highlight smalltalk linenos %}
ExampleSetTest >>
testOccurrences
    self assert: (empty occurrencesOf: 0) = 0.
    self assert: (full occurrencesOf: 5) = 1.
    full add: 5.
    self assert: (full occurrencesOf: 5) = 1
{% endhighlight %}

{% highlight smalltalk linenos %}
ExampleSetTest >> 
testRemove
    full remove: 5.
    self assert: (full includes: 6).
    self deny: (full includes: 5)
{% endhighlight %}

#### Step 4: run the tests

The easiest way to run the tests is directly from the browser. Simply action- click on the package, class name, or on an individual test method, and select **run the tests (t)**. 

#### Step 5: interpret the results

The method assert: , which is defined in the class TestCase, expects a boolean argument, usually the value of a tested expression. When the argument is true, the test passes; when the argument is false, the test fails.

### The SUnit cook book
---

#### Other assertions

In addition to assert: and deny:, there are several other methods that can be used to make assertions.

* **First**, assert:description: and deny:description: take a second argument which is a message string that can be used to describe the reason for the failure, if it is not obvious from the test itself.
* **Next**, SUnit provides two additional methods, should:raise: and shouldnt:raise: for testing exception propagation. For example, you would use (self should: aBlock raise: anException) to test that a particular exception is raised during the execution of aBlock. 

{% highlight smalltalk linenos %}
ExampleSetTest >> 
testIllegal
    self should: [empty at: 5] raise: Error.
    self should: [empty at: 5 put: #zork] raise: Error
{% endhighlight %}

#### Running a single test

Normally, you will run your tests using the Test Runner. If you don’t want to launch the Test Runner from the menu, you can execute TestRunner open as a ￼**print it**.
￼
{% highlight smalltalk linenos %}
ExampleSetTest run: #testRemove 
--->
1 run, 1 passed, 0 failed, 0 errors￼￼￼
{% endhighlight %}

#### Running all the tests in a test class

{% highlight smalltalk linenos %}
ExampleSetTest suite run
--->
5 run, 5 passed, 0 failed, 0 errors Mu
{% endhighlight %}

#### Must I subclass TestCase?

In JUnit you can build a TestSuite from an arbitrary class containing test\* methods. In Smalltalk you can do the same but you will then have to create a suite by hand and your class will have to implement all the essential TestCase methods like assert:. We recommend that you not try to do this. The framework is there: use it.

### The SUnit framework
---

SUnit consists of four main classes: TestCase, TestSuite, TestResult, and TestResource.

![Figure 7.3](/assets/img/smalltalk/Figure 7.3.png)

The four classes representing the core of SUnit

#### TestCase

TestCase is an abstract class that is designed to be subclassed; each of its subclasses represents a group of tests that share a common context (that is, a test suite). Each test is run by creating a new instance of a subclass of TestCase, running setUp, running the test method itself, and then running tearDown.
The context is specified by instance variables of the subclass and by the specialization of the method setUp, which initializes those instance variables. Subclasses of TestCase can also override method tearDown, which is invoked after the execution of each test, and can be used to release any objects allocated during setUp.

#### TestSuite

Instances of the class TestSuite contain a collection of test cases. An instance of TestSuite contains tests, and other test suites. That is, a test suite contains sub- instances of TestCase and TestSuite. Both individual TestCases and TestSuites understand the same protocol, so they can be treated in the same way; for example, both can be run. This is in fact an application of the composite pattern in which TestSuite is the composite and the TestCases are the leaves — see Design Patterns for more information on this pattern2.

#### TestResult

The class TestResult represents the results of a TestSuite execution. It records the number of tests passed, the number of tests failed, and the number of errors signalled.

#### TestResource

One of the important features of a suite of tests is that they should be inde- pendent of each other: the failure of one test should not cause an avalanche of failures of other tests that depend upon it, nor should the order in which the tests are run matter. Performing setUp before each test and tearDown afterwards helps to reinforce this independence.

### Advanced features of SUnit
---

In addition to TestResource, the current version of SUnit contains **  assertion description strings**, **logging support**, and **resumable test failures**.

#### Assertion description strings
The TestCase assertion protocol includes a number of methods that allow the programmer to supply a description of the assertion. The description is a String; if the test case fails, this string will be displayed by the test runner. Of course, this string can be constructed dynamically.

#### Logging support
The description strings described above may also be logged to a Stream such as the Transcript, or a file stream. You can choose whether to log by overriding TestCase»isLogging in your test class; you must also choose where to log by overriding TestCase»failureLog to answer an appropriate stream.

#### Continuing after a failure
SUnit also allows us to specify whether or not a test should continue after a failure. This is a really powerful feature that uses the exception mechanisms offered by Smalltalk.

### The implementation of SUnit
---

#### Running one test

![Figure 7.4](/assets/img/smalltalk/Figure 7.4.png)

Running one test

#### Running a TestSuite

### Some advice on testing
---

* **Feathers’ Rules** for Unit tests. Michael Feathers, an agile process consultant and author, writes:3
A test is not a unit test if:

    * it talks to the database,
    * it communicates across the network,
    * it touches the file system,
    * it can’t run at the same time as any of your other unit tests, or
    * you have to do special things to your environment (such as editing config files) to run it.

* **Unit Tests vs. Acceptance Tests.**
Unit tests capture one piece of functionality, and as such make it easier to identify bugs in that functionality. As far as possible try to have unit tests for each method that could possibly fail, and group them per class.
However, for certain deeply recursive or complex setup situations, it is easier to write tests that represent a scenario in the larger application; these are called acceptance tests or functional tests. Tests that break Feathers’ rules may make good acceptance tests. Group acceptance tests according to the functionality that they test. For example, if you are writing a compiler, you might write acceptance tests that make assertions about the code generated for each possible source language statement. Such tests might exercise many classes, and might take a long time to run because they touch the file system.
You can write them using SUnit, but you won’t want to run them each time you make a small change, so they should be separated from the true unit tests.

* **Black’s Rule of Testing.**
For every test in the system, you should be able to identify some property for which the test increases your confidence.
It’s obvious that there should be no important property that you are not testing. This rule states the less obvious fact that there should be no test that does not add value to the system by increasing your confidence that a useful property holds.
For example, several tests of the same property do no good. In fact, they do harm in two ways. First, they make it harder to infer the behaviour of the class by reading the tests. Second, because one bug in the code might then break many tests, they make it harder to estimate how many bugs remain in the code. So, have a property in mind when you write a test.

## Chapter summary
---

This chapter explained why tests are an important investment in the future of your code. We explained in a step-by-step fashion how to define a few tests for the class Set. Then we gave an overview of the core of the SUnit framework by presenting the classes TestCase, TestResult, TestSuite and TestResources. Finally we looked deep inside SUnit by following the execution of a test and a test suite.

* To maximize their potential, unit tests should be fast, repeatable, in- dependent of any direct human interaction and cover a single unit of functionality.
* Tests for a class called MyClass belong in a class classed MyClassTest, which should be introduced as a subclass of TestCase.
* Initialize your test data in a setUp method.
* Each test method should start with the word “test”.
* Use the TestCase methods assert:, deny: and others to make assertions.
* Run tests using the SUnit test runner tool (in the tool bar).
