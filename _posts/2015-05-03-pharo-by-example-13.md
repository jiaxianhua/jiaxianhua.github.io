---
layout: post
title: "Pharo by Example 13"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## Classes and metaclasses
---

1. Rule 1. Everything is an object.
1. Rule 2. Every object is an instance of a class.
1. Rule 3. Every class has a superclass.
1. Rule 4. Everything happens by sending messages.
1. Rule 5. Method lookup follows the inheritance chain.
1. Rule 6. Every class is an instance of a metaclass.
1. Rule 7. The metaclass hierarchy parallels the class hierarchy.
1. Rule 8. Every metaclass inherits from Class and Behavior.
1. Rule 9. Every metaclass is an instance of Metaclass.
1. Rule 10. The metaclass of Metaclass is an instance of Metaclass.

### Revisiting the Smalltalk object model
---
Since everything is an object, the color blue in Smalltalk is also an object.

{% highlight smalltalk %}
Color blue ---> Color blue
{% endhighlight %}

Every object is an instance of a class. The class of the color blue is the class Color:

{% highlight smalltalk %}
Color blue class ---> Color
{% endhighlight %}

Interestingly, if we set the alpha value of a color, we get an instance of a different class, namely TranslucentColor:

{% highlight smalltalk %}
(Color blue alpha: 0.4) class ---> TranslucentColor
{% endhighlight %}

We can create a morph and set its color to this translucent color:

{% highlight smalltalk %}
EllipseMorph new color: (Color blue alpha: 0.4); openInWorld
{% endhighlight %}

![Figure 13.1](/assets/img/smalltalk/Figure 13.1.png)

A translucent ellipse

By Rule 3, every class has a superclass. The superclass of TranslucentColor
is Color, and the superclass of Color is Object:

{% highlight smalltalk %}
TranslucentColor superclass ---> Color
Color superclass ---> Object
{% endhighlight %}

Everything happens by sending messagess (Rule 4), so we can deduce that blue is a message to Color, class and alpha: are messages to the color blue, openInWorld is a message to an ellipse morph, and superclass is a message to TranslucentColor and Color. The receiver in each case is an object, since everything is an object, but some of these objects are also classes.

Method lookup follows the inheritance chain (Rule 5), so when we send the message class to the result of Color blue alpha: 0.4, the message is handled when the corresponding method is found in the class Object.

![Figure 13.2](/assets/img/smalltalk/Figure 13.2.png)

Sending a message to a translucent color

The figure captures the essence of the is-a relationship. Our translucent blue object is a TranslucentColor instance, but we can also say that it is a Color and that it is an Object, since it responds to the messages defined in all of these classes. In fact, there is a message, isKindOf:, that you can send to any object to find out if it is in an is a relationship with a given class:

{% highlight smalltalk %}
translucentBlue := Color blue alpha: 0.4.
translucentBlue isKindOf: TranslucentColor ---> true
translucentBlue isKindOf: Color ---> true
translucentBlue isKindOf: Object ---> true
{% endhighlight %}

### Every class is an instance of a metaclass
---

classes whose instances are themselves classes are called metaclasses.

**Metaclasses are implicit**. Metaclasses are automatically created when you define a class. We say that they are implicit since as a programmer you never have to worry about them. An implicit metaclass is created for each class you create, so each metaclass has only a single instance.

Whereas ordinary classes are named by global variables, metaclasses are anonymous. However, we can always refer to them through the class that is their instance. The class of Color, for instance, is Color class, and the class of Object is Object class:

{% highlight smalltalk %}
Color class ---> Color class
Object class ---> Object class
{% endhighlight %}

![Figure 13.3](/assets/img/smalltalk/Figure 13.3.png)

The metaclasses of Translucent and its superclasses

The fact that classes are also objects makes it easy for us to query them by sending messages. Let’s have a look:

{% highlight smalltalk %}
Color subclasses ---> {TranslucentColor}
TranslucentColor subclasses ---> #()
TranslucentColor allSuperclasses ---> an OrderedCollection(Color Object ProtoObject)
TranslucentColor instVarNames ---> #('alpha')
TranslucentColor allInstVarNames ---> #('rgb' 'cachedDepth' 'cachedBitPattern' ' alpha')
TranslucentColor selectors ---> an IdentitySet(#pixelValueForDepth:  #pixelWord32 #convertToCurrentVersion:refStream: #isTransparent #scaledPixelValue32 #bitPatternForDepth: #storeArrayValuesOn: #setRgb:alpha: #alpha #isOpaque #pixelWordForDepth: #isTranslucentColor #hash #isTranslucent #alpha: #storeOn: #asNontranslucentColor #privateAlpha #balancedPatternForDepth:)
{% endhighlight %}

### The metaclass hierarchy parallels the class hierarchy
---


Rule 7 says that the superclass of a metaclass cannot be an arbitrary class: it is constrained to be the metaclass of the superclass of the metaclass’s unique instance.

{% highlight smalltalk %}
TranslucentColor class superclass ---> Color class TranslucentColor 
superclass class ---> Color class
{% endhighlight %}

This is what we mean by the metaclass hierarchy being parallel to the class hierarchy; Figure 13.4 shows how this works in the TranslucentColor hierarchy.

![Figure 13.4](/assets/img/smalltalk/Figure 13.4.png)

The metaclass hierarchy parallels the class hierarchy.

{% highlight smalltalk %}
TranslucentColor class ---> TranslucentColor class
TranslucentColor class superclass ---> Color class
TranslucentColor class superclass superclass ---> Object class
{% endhighlight %}

**Uniformity between Classes and Objects.**
It is interesting to step back a moment and realize that there is no difference between sending a message to an object and to a class. In both cases the search for the corresponding method starts in the class of the receiver, and proceeds up the inheritance chain.

Thus, messages sent to classes must follow the metaclass inheritance chain. Consider, for example, the method blue, which is implemented on the class side of Color. If we send the message blue to TranslucentColor, then it will be looked-up the same way as any other message. The lookup starts in TranslucentColor class, and proceeds up the metaclass hierarchy until it is found in Color class (see Figure 13.5).

{% highlight smalltalk %}
TranslucentColor blue ---> Color blue
{% endhighlight %}

Note that we get as a result an ordinary Color blue, and not a translucent one — there is no magic!

![Figure 13.5](/assets/img/smalltalk/Figure 13.5.png)

Message lookup for classes is the same as for ordinary objects.

Thus we see that there is one uniform kind of method lookup in Smalltalk. Classes are just objects, and behave like any other objects. Classes have the power to create new instances only because classes happen to respond to the message new, and because the method for new knows how to create new instances. Normally, non-class objects do not understand this message, but if you have a good reason to do so, there is nothing stopping you from adding a new method to a non-metaclass.

Since classes are objects, we can also inspect them.

> Inspect Color blue and Color.

Notice that in one case you are inspecting an instance of Color and in the other case the Color class itself. This can be a bit confusing, because the title bar of the inspector names the class of the object being inspected.

The inspector on Color allows you to see the superclass, instance variables, method dictionary, and so on, of the Color class.

![Figure 13.6](/assets/img/smalltalk/Figure 13.6.png)

Classes are objects too.

### Every metaclass Inherits from Class and Behavior
---

Every metaclass is-a class, hence inherits from Class. Class in turn inherits from its superclasses, ClassDescription and Behavior. Since everything in Smalltalk is-an object, these classes all inherit eventually from Object. 

**Where is new defined?** To understand the importance of the fact that metaclasses inherit from Class and Behavior, it helps to ask where new is defined and how it is found. When the message new is sent to a class it is looked up in its metaclass chain and ultimately in its superclasses Class, ClassDescription and Behavior.

![Figure 13.7](/assets/img/smalltalk/Figure 13.7.png)

Metaclasses inherit from Class and Behavior

The question *“Where new is defined?”* is crucial. **new** is first defined in the class **Behavior**, and it can be redefined in its subclasses, including any of the metaclass of the classes we define, when this is necessary. Now when a message new is sent to a class it is looked up, as usual, in the metaclass of this class, continuing up the superclass chain right up to the class Behavior, if it has not been redefined along the way.

Note that the result of sending TranslucentColor new is an instance of TranslucentColor and not of Behavior, even though the method is looked-up in the class Behavior! new always returns an instance of self, the class that receives the message, even if it is implemented in another class.

{% highlight smalltalk %}
TranslucentColor new class ---> TranslucentColor "not Behavior"
{% endhighlight %}

A common mistake is to look for new in the superclass of the receiving class. The same holds for new:, the standard message to create an object of a given size. For example, **Array new: 4** creates an array of 4 elements. You will not find this method defined in Array or any of its superclasses. Instead you should look in Array class and its superclasses, since that is where the lookup will start.

![Figure 13.8](/assets/img/smalltalk/Figure 13.8.png)

new is an ordinary message looked up in the metaclass chain.

**Responsibilities of Behavior, ClassDescription and Class.** Behavior provides the minimum state necessary for objects that have instances: this includes a superclass link, a method dictionary, and a description of the instances (i.e., representation and number). Behavior inherits from Object,so it,and all of its subclasses, can behave like objects.

Behavior is also the basic interface to the compiler. It provides methods for creating a method dictionary, compiling methods, creating instances (i.e., new, basicNew, new:, and basicNew:), manipulating the class hierarchy (i.e., superclass:, addSubclass:), accessing methods (i.e., selectors, allSelectors, compiledMethodAt:), accessing instances and variables (i.e., allInstances, instVarNames . . . ), accessing the class hierarchy (i.e., superclass, subclasses) and querying (i.e., hasMethods, includesSelector, canUnderstand:, inheritsFrom:, isVariable).

ClassDescription is an abstract class that provides facilities needed by its two direct subclasses, Class and Metaclass. ClassDescription adds a number of facilities to the basis provided by Behavior: named instance variables, the categorization of methods into protocols, the notion of a name (abstract), the maintenance of change sets and the logging of changes, and most of the mechanisms needed for filing-out changes.

Class represents the common behaviour of all classes. It provides a class name, compilation methods, method storage, and instance variables. It provides a concrete representation for class variable names and shared pool variables (addClassVarName:, addSharedPool:, initialize). Class knows how to create instances, so all metaclasses should inherit ultimately from Class.

![Figure 13.9](/assets/img/smalltalk/Figure 13.9.png)

Every metaclass is a Metaclass.

### Every metaclass is an instance of Metaclass
---

Metaclasses are objects too; they are instances of the class Metaclass. The instances of class Metaclass are the anonymous metaclasses, each of which has exactly one instance, which is a class.

Metaclass represents common metaclass behaviour. It provides methods for instance creation (subclassOf:) creating initialized instances of the metaclass’s sole instance, initialization of class variables, metaclass instance, method compilation, and class information (inheritance links, instance variables, etc.).

### The metaclass of Metaclass is an Instance of Metaclass
---

The final question to be answered is: *what is the class of Metaclass class?*

The answer is simple: it is a metaclass, so it must be an instance of Metaclass, just like all the other metaclasses in the system.

The figure shows how all metaclasses are instances of Metaclass, including the metaclass of Metaclass itself.

![Figure 13.10](/assets/img/smalltalk/Figure 13.10.png)

All metaclasses are instances of the class Metaclass, even the metaclass of Metaclass.

will see how the metaclass hierarchy perfectly mirrors the class hierarchy, all the way up to Object class.

The following examples show us how we can query the class hierarchy to demonstrate that Figure 13.10 is correct. (Actually, you will see that we told a white lie — Object class superclass ≠æ ProtoObject class, not Class. In Pharo, we must go one superclass higher to reach Class.)

Example 13.1: The class hierarchy TranslucentColor superclass ≠æ Color

{% highlight smalltalk %}
Color superclass ---> Object
{% endhighlight %}

Example 13.2: The parallel metaclass hierarchy

{% highlight smalltalk %}
TranslucentColor class superclass ---> Color class
Color class superclass ---> Object class
Object class superclass superclass ---> Class "NB: skip ProtoObject class"
Class superclass ---> ClassDescription
ClassDescription superclass ---> Behavior
Behavior superclass ---> Object
{% endhighlight %}

Example 13.3: Instances of Metaclass

{% highlight smalltalk %}
TranslucentColor class class ---> Metaclass
Color class class ---> Metaclass
Object class class ---> Metaclass
Behavior class class ---> Metaclass
{% endhighlight %}
￼￼
Example 13.4: Metaclass class is a Metaclass

{% highlight smalltalk %}
Metaclass class class ---> Metaclass
Metaclass superclass ---> ClassDescription
{% endhighlight %}

## Chapter summary
---


Now you should understand better how classes are organized and the impact of a uniform object model. If you get lost or confused, you should always remember that message passing is the key: you look for the method in the class of the receiver. This works on any receiver. If the method is not found in the class of the receiver, it is looked up in its superclasses.

* Every class is an instance of a metaclass. Metaclasses are implicit. A Metaclass is created automatically when you create the class that is its sole instance.
* The metaclass hierarchy parallels the class hierarchy. Method lookup for classes parallels method lookup for ordinary objects, and follows the metaclass’s superclass chain.
* Every metaclass inherits from Class and Behavior. Every class is a Class . Since metaclasses are classes too, they must also inherit from Class. Behavior provides behaviour common to all entities that have instances.
* Every metaclass is an instance of Metaclass. ClassDescription provides everything that is common to Class and Metaclass.
* The metaclass of Metaclass is an instance of Metaclass. The instance-of relation forms a closed loop, so Metaclass class class ---> Metaclass.

