---
layout: post
title: "swift 关键字和符号"
description: ""
category: swift
tags: [swift]
---
{% include JB/setup %}

# keywords and identifiers
---

Keywords used in declarations: 

{% highlight swift %}
class, deinit, enum, extension, func, import, init, inout, internal, let, operator, private, protocol, public, static, struct, subscript, typealias, var
{% endhighlight %}

Keywords used in statements: 

{% highlight swift %}
break, case, continue, default, defer, do, else, fallthrough, for, guard, if, in, repeat, return, switch, where, while
{% endhighlight %}

Keywords used in expressions and types:

{% highlight swift %}
as, catch, dynamicType, false, is, nil, rethrows, super, self, Self, throw, throws, true, try, __COLUMN__, __FILE__, __FUNCTION__, __LINE__
{% endhighlight %}

Keywords used in patterns: 

{% highlight swift %}
_
{% endhighlight %}

Keywords reserved in particular contexts:

{% highlight swift %}
associativity, convenience, dynamic, didSet, final, get, infix, indirect, lazy, left, mutating, none, nonmutating, optional, override, postfix, precedence, prefix, Protocol, required, right, set, Type, unowned, weak, willSet
{% endhighlight %}

> Outside the context in which they appear in the grammar, they can be used as identifiers.

The following tokens are reserved as punctuation and can’t be used as custom operators: 

{% highlight swift %}
(, ), {, }, [, ], ., ,, :, ;, =, @, #, & (as a prefix operator), ->, `, ?, and ! (as a postfix operator)
{% endhighlight %}

# 关键字和符号
---

下面这些被保留的关键字(keywords)不允许用作标识符,除非被反引号转义,具体描述请参考 标识符。

* 用在声明中的关键字: 

{% highlight swift %}
class、deinit、enum、extension、func、import、init、let、protocol、static、struct、subscript、typealias、var
{% endhighlight %}


* 用在语句中的关键字:
{% highlight swift %}
break、case、continue、default、do、else、fallthrough、if、in、for、return、switch、where、while
{% endhighlight %}
* 用在表达式和类型中的关键字:

{% highlight swift %}
as、dynamicType、is、new、super、self、Self、Type、__COLUMN__、__FILE__、__FUNCTION__、__LINE__
{% endhighlight %}

* 用在模式中的关键字:

{% highlight swift %}
_
{% endhighlight %}

* 特定上下文中被保留的关键字: 

{% highlight swift %}
associativity、didSet、get、infix、inout、left、mutating、none、nonmutating、operator、override、postfix、precedence、prefix、right、set、unowned、unowned(safe)、unowned(unsafe)、weak、willSet
{% endhighlight %}

> 这些关键字在特定上下文之外可以被用于标识符。

* 以下标记被当作保留符号,不能用于自定义操作符:
{% highlight swift %}
 ( 、 ) 、 { 、 } 、 [ 、 ] 、 . 、 , 、 : 、 ; 、 = 、 @ 、 # 、 &(作为前缀操作符) 、 -> 、`` 、 ? 和 !(作为后缀操作符)`。
{% endhighlight %}

