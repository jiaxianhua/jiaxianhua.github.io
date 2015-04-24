---
layout: post
title: "Pharo by Example 3"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## Syntax in a nutshell
---

* Syntactic elements

> (i) six reserved keywords, or pseudo-variables: self, super, nil, true, false, and thisContext,  
> (ii) constant expressions for literal objects including numbers, characters, strings, symbols and arrays,   
> (iii) variable declarations,   
> (iv) assignments,   
> (v) block closures, and   
> (vi) messages.  

### Pharo Syntax in a Nutshell
---

| Syntax | What it represents |
|---+---|
| startPoint | a variable name |
| Transcript  | a global variable name |
| self | pseudo-variable |
| 1 | decimal integer  |
| 2r101 | binary integer  |
| 1.5 | floating point number  |
| 2.4e7 | exponential notation  |
| $a | the character 'a' |
| 'Hello'  | the string "Hello"  |
| #Hello | the symbol #Hello  |
| #(1 2 3) | a literal array |
| {1. 2. 1+2} | a dynamic array |
| "a comment" | a comment |
| \| x y \| | declaration of variables x and y |
| x := 1 | assign 1 to x |
| [x+y] | a block that evaluates to x+y |
| <primitive: 1> | virtual machine primitive or annotation |
| 3 factorial | unary message |
| 3+4 | binary messages  |
| 2 raisedTo: 6 modulo: 10 | keyword message |
| ^ true | return the value true  |
| Transcript show: ’hello’. Transcript cr  | expression separator (.) |
| Transcript show: ’hello’; cr | message cascade (;) |

---

**Local variables** startPoint is a variable name, or identifier. By convention, identifiers are composed of words in “camelCase” (i.e., each word except the first starting with an upper case letter). The first letter of an instance variable, method or block argument, or temporary variable must be lower case. This indicates to the reader that the variable has a private scope.

**Shared variables** Identifiers that start with upper case letters are global variables, class variables, pool dictionaries or class names. Transcript is a global variable, an instance of the class TranscriptStream.

**The receiver** self is a keyword that refers to the object inside which the current method is executing. We call it “the receiver” because this object will normally have received the message that caused the method to execute. self is called a “pseudo-variable” since we cannot assign to it.

**Integers** Inadditiontoordinarydecimalintegerslike42,Pharoalsoprovides a radix notation. 2r101 is 101 in radix 2 (i.e., binary), which is equal to decimal 5.

**Floating point numbers** can be specified with their base-ten exponent: 2.4e7 is 2.4 ^ 107.

**Characters** A dollar sign introduces a literal character: $a is the literal for ‘a’. Instances of non-printing characters can be obtained by sending appropriately named messages to the Character class, such as Character space and Character tab.

**Strings** Single quotes are used to define a literal string. If you want a string with a quote inside, just double the quote, as in 'G''day'.

**Symbols** are like Strings, in that they contain a sequence of characters. However, unlike a string, a literal symbol is guaranteed to be globally unique. There is only one Symbol object #Hello but there may be multiple String objects with the value 'Hello'.

**Compile-time arrays** are defined by #( ), surrounding space-separated literals. Everything within the parentheses must be a compile-time constant. For example, #(27 (true false) abc) is a literal array of three elements: the integer 27, the compile-time array containing the two booleans, and the symbol #abc. (Note that this is the same as #(27 #(true false) #abc).)

**Run-time arrays** Curly braces { } define a (dynamic) array at run-time. Elements are expressions separated by periods. So { 1. 2. 1+2 } defines an array with elements 1, 2, and the result of evaluating 1+2. (The curlybrace notation is peculiar to the Pharo and Squeak dialects of Smalltalk! In other Smalltalks you must build up dynamic arrays explicitly.)

**Comments** are enclosed in double quotes. "hello" is a comment, not a string, and is ignored by the Pharo compiler. Comments may span multiple lines.

**Local variable definitions** Vertical bars | | enclose the declaration of one or more local variables in a method (and also in a block).

**Assignment** := assigns an object to a variable.

**Blocks** Square brackets [ ] define a block, also known as a block closure or a lexical closure, which is a first-class object representing a function. As we shall see, blocks may take arguments and can have local variables.

**Primitives** \<primitive:...\> denotes an invocation of a virtualmachine primitive. (<primitive: 1> is the VM primitive for SmallInteger»+.) Any code following the primitive is executed only if the primitive fails. The same syntax is also used for method annotations.

**Unary messages** consist of a single word (like factorial) sent to a receiver (like 3).

**Binary messages** are operators (like +) sent to a receiver and taking a single argument. In 3+4, the receiver is 3 and the argument is 4.

**Keyword messages** consist of multiple keywords (like raisedTo:modulo:), each ending with a colon and taking a single argument. In the expression 2 raisedTo: 6 modulo: 10, the message selector raisedTo:modulo: takes the two arguments 6 and 10, one following each colon. We send the message to the receiver 2.

**Method return** *^* is used to return a value from a method.

**Sequences of statements** A period or full-stop (.) is the statement separator. Putting a period between two expressions turns them into independent statements.

**Cascades** Semicolons can be used to send a cascade of messages to a single receiver. In Transcript show: 'hello'; cr we first send the keyword message show: 'hello' to the receiver Transcript, and then we send the unary message cr to the same receiver.

### Message sends
---

1. **Unary messages** take no argument. 1 factorial sends the message factorial
to the object 1.
2. **Binary messages** take exactly one argument. 1 + 2 sends the message +
with argument 2 to the object 1.
3. **Keyword messages** take an arbitrary number of arguments. 2 raisedTo: 6 modulo: 10 sends the message consisting of the message selector
raisedTo:modulo: and the arguments 6 and 10 to the object 2.

* Unary message selectors consist of alphanumeric characters, and start
with a lower case letter.
* Binary message selectors consist of one or more characters from the following set:

    `+−/\*~<>=@%|& ?,`

* Keyword message selectors consist of a series of alphanumeric keywords,
where each keyword starts with a lower-case letter and ends with a colon.
* Unary messages have the highest precedence, then binary messages, and finally keyword messages.

### Method syntax
---

{% highlight smalltalk linenos %}
lineCount
	"Answer the number of lines represented by the receiver, where every cr adds one line."
	| cr count |
	cr := Character cr.
	count := 1 min: self size. self do:
	[:c | c == cr ifTrue: [count := count + 1]].
    ^ count
{% endhighlight %}

1. the method pattern, containing the name (i.e., lineCount) and any arguments (none in this example); 
2. comments (these may occur anywhere, but the convention is to put one at the top that explains what the method does);
3. declarations of local variables (i.e., cr and count); and
4. any number of expressions separated by dots; here there are four.

### Block syntax
---

**块本质上是匿名函数,实际上是词法闭包**

Blocks provide a mechanism to defer the evaluation of expressions. A block is essentially an anonymous function. A block is evaluated by sending it the message value. The block answers the value of the last expression in its body, unless there is an explicit return (with ø), in which case it does not answer any value.

{% highlight smalltalk linenos %}
[1+2] value -> 3
{% endhighlight %}

Blocks may take parameters, each of which is declared with a leading colon. A vertical bar separates the parameter declaration(s) from the body of the block. To evaluate a block with one parameter, you must send it the message value: with one argument. A two-parameter block must be sent value:value:, and so on, up to 4 arguments.

{% highlight smalltalk linenos %}
[:x|1+x]value:2 -> 3
[:x:y|x+y] value:1 value:2 -> 3
{% endhighlight %}

### Conditionals and loops in a nutshell
---


1. Smalltalk offers no special syntax for control constructs. Instead, these are typically expressed by sending messages to booleans, numbers and collections, with blocks as arguments.

1. Conditionals are expressed by sending one of the messages ifTrue:, ifFalse: or ifTrue:ifFalse: to the result of a boolean expression.

{% highlight smalltalk linenos %}
(17 * 13 > 220)
    ifTrue: [ 'bigger' ]
    ifFalse: [ 'smaller' ]
    -> 'bigger'
{% endhighlight %}

1. Loops are typically expressed by sending messages to blocks, integers or collections. Since the exit condition for a loop may be repeatedly evaluated, it should be a block rather than a boolean value. Here is an example of a very procedural loop:

{% highlight smalltalk linenos %}
n := 1.
    [ n < 1000 ] whileTrue: [ n := n*2 ].
    n -> 1024
{% endhighlight %}

### Primitives and pragmas
---


1. In Smalltalk everything is an object, and everything happens by sending messages. Nevertheless, at certain points we hit rock bottom. Certain objects can only get work done by invoking virtual machine primitives.

1. For example, the following are all implemented as primitives: memory allocation (new, new:), bit manipulation (bitAnd:, bitOr:, bitShift:), pointer and integer arithmetic (+, −, <, >, \*, / , =, ==...), and array access (at:, at:put:).

1. Primitives are invoked with the syntax <primitive: aNumber>. A method that invokes such a primitive may also include Smalltalk code, which will be evaluated only if the primitive fails.


## Chapter summary
---

* Pharo has (only) six reserved identifiers also called pseudo-variables: true, false, nil, self, super, and thisContext.
* There are five kinds of literal objects: numbers (5, 2.5, 1.9e15, 2r111), characters ($a), strings ('hello'), symbols (#hello), and arrays (#('hello' #hi))
* Strings are delimited by single quotes, comments by double quotes. To get a quote inside a string, double it.
* Unlike strings, symbols are guaranteed to be globally unique.
* Use #( ... ) to define a literal array. Use { ... } to define a dynamic array.
`Notethat#(1+2) size` -> 3, but `{1+2} size` -> 1
* There are three kinds of messages: unary (e.g., 1 asString, Array new), binary (e.g., 3 + 4, 'hi' , ' there'), and keyword (e.g., 'hi' at: 2 put: $o)
* A cascaded message send is a sequence of messages sent to the same target, separated by semi-colons: 
`OrderedCollection new add: #calvin; add: #hobbes; size` -> 2 
* Local variables are declared with vertical bars. Use := for assignment.  `|x| x:=1` 
* Expressions consist of message sends, cascades and assignments, possibly grouped with parentheses. Statements are expressions separated by periods.
* Block closures are expressions enclosed in square brackets. Blocks may take arguments and can contain temporary variables. The expressions in the block are not evaluated until you send the block a value... message with the correct number of arguments.
* There is no dedicated syntax for control constructs, just messages that conditionally evaluate blocks.
`(Smalltalk includes: Class) ifTrue: [ Transcript show: Class superclass ]`
