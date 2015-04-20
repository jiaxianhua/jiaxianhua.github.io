---
layout: post
title: "I Can Read C++ and Java But I Can’t Read Smalltalk"
description: ""
category: smalltalk 
tags: [smalltalk, language]
---
{% include JB/setup %}

原文:<http://www.eli.sdsu.edu/courses/spring01/cs635/readingSmalltalk.pdf>
标题: I Can Read C++ and Java But I Can’t Read Smalltalk
作者: Wilf LaLonde

{% highlight smalltalk %}
"this is a commit"
'this is a string'
#'This is a symbol'
#ThisIsSymbolToo'
{% endhighlight %}

> := // Means assignment
> = //Means equality comparison
> == //Means identity comparison

{% highlight smalltalk %}
#(1 2 3 4 5)
"string1string2"
'string1', 'string2' 
{% endhighlight %}

## Keywords Are Pervasive
---

{% highlight smalltalk %}
| t a v |
| aTransformation angle aVector|

self rotateBy: angle around: vector
|result|
result := COMPUTE ANWSER.
^result
{% endhighlight %}
