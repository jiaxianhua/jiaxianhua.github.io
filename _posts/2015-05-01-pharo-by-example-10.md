---
layout: post
title: "Pharo by Example 10"
description: ""
category: smalltalk
tags: [smalltalk, book]
---
{% include JB/setup %}

## Streams
---

### Two sequences of elements
---

![Figure 10.1](/assets/img/smalltalk/Figure 10.1.png)

A stream positioned at its beginning.

![Figure 10.2](/assets/img/smalltalk/Figure 10.2.png)

The same stream after the execution of the method next: the character a is “in the past” whereas b, c, d and e are “in the future”.

![Figure 10.3](/assets/img/smalltalk/Figure 10.3.png)

The same stream after having written an x.

### Streams vs. collections
---

The collection protocol supports the storage, removal and enumeration of the elements of a collection, but does not allow these operations to be intermingled. For example, if the elements of an OrderedCollection are processed by a do: method, it is not possible to add or remove elements from inside the do: block. Nor does the collection protocol offer ways to iterate over two collections at the same time, choosing which collection goes forward and which does not. Procedures like these require that a traversal index or position reference is maintained outside of the collection itself: this is exactly the role of ReadStream, WriteStream and ReadWriteStream.

These three classes are defined to stream over some collection. For example, the following snippet creates a stream on an interval, then it reads two elements.

{% highlight smalltalk %}
r := ReadStream on: (1 to: 1000).
r next.  ---> 1
r next.  ---> 2
r atEnd.  ---> false
{% endhighlight %}

WriteStreams can write data to the collection:

{% highlight smalltalk %}
w := WriteStream on: (String new: 5). w nextPut: $a.
w nextPut: $b.
w contents. ---> 'ab'
{% endhighlight %}

### Streaming over collections
---

#### Reading collections

This section presents features used for reading collections. Using a stream to read a collection essentially provides you a pointer into the collection. That pointer will move forward on reading and you can place it wherever you want. The class ReadStream should be used to read elements from collections.

Methods next and next: are used to retrieve one or more elements from the collection.

{% highlight smalltalk %}
stream := ReadStream on: #(1 (a b c) false).
stream next. ---> 1
stream next. ---> #(#a #b #c)
stream next. ---> false

stream := ReadStream on: 'abcdef'.
stream next: 0. ---> ''
stream next: 1. ---> 'a'
stream next: 3. ---> 'bcd'
stream next: 2. ---> 'ef'
{% endhighlight %}

#### Writing to collections

We have already seen how to read a collection by iterating over its elements using a ReadStream. We’ll now learn how to create collections using WriteStreams.

WriteStreams are useful for appending a lot of data to a collection at various locations. They are often used to construct strings that are based on static and dynamic parts as in this example:

{% highlight smalltalk %}
stream := String new writeStream.
stream
    nextPutAll: 'This Smalltalk image contains: ';
    print: Smalltalk allClasses size;
    nextPutAll: ' classes.';
    cr;
    nextPutAll: 'This is really a lot.'.

stream contents. ---> 'This Smalltalk image contains: 2322 classes. This is really a lot.'
{% endhighlight %}

**nextPut**: adds the parameter to the stream;
**nextPutAll**: add seach element of the collection. Passed as a parameter, to the stream;
**print**: adds the textual representation of the parameter to the stream.

**About Concatenation**. Using nextPut: and nextPutAll: on a WriteStream is often the best way to concatenate characters. Using the comma concatenation operator (,) is far less efficient:

{% highlight smalltalk %}

[| temp |
    temp := String new.
    (1 to: 100000)
    do: [ :i | temp := temp, i asString, '']] timeToRun ---> 115176 "(milliseconds)"

[| temp |
    temp := WriteStream on: String new.
    (1 to: 100000)
    do: [ :i | temp nextPutAll: i asString; space].
    temp contents] timeToRun ---> 1262 "(milliseconds)"
{% endhighlight %}

#### Reading and writing at the same time

![Figure 10.5](/assets/img/smalltalk/Figure 10.5.png)

Reading and writing at the same time

![Figure 10.9](/assets/img/smalltalk/Figure 10.9.png)


### Using streams for file access
---

#### Creating file streams
To create file streams, you will have to use one of the following instance creation methods offered by the class FileStream:

* **fileNamed**: Open a file with the given name for reading and writing. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close. If the name has no directory part, then the file will be created in the default directory.

* **newFileNamed**: Create a new file with the given name,and answer a stream opened for writing on that file. If the file already exists, ask the user what to do.

* **forceNewFileNamed**: Create a new file with the given name, and answer a stream opened for writing on that file. If the file already exists, delete it without asking before creating the new file.

* **oldFileNamed**: Open an existing file with the given name for reading and writing. If the file already exists, its prior contents may be modified or replaced, but the file will not be truncated on close. If the name has no directory part, then the file will be created in the default directory.

* **readOnlyFileNamed**: Open an existing file with the given name for reading.

#### Binary streams

By default, created streams are text-based which means you will read and write characters. If your stream must be binary, you have to send the message binary to your stream.

When your stream is in binary mode, you can only write numbers from 0 to 255 (1 Byte). If you want to use nextPutAll: to write more than one number at a time, you have to pass a ByteArray as argument.

{% highlight smalltalk %}
FileStream
    forceNewFileNamed: 'test.bin'
    do: [:stream |
        stream
        binary;
        nextPutAll: #(145 250 139 98) asByteArray].

FileStream
readOnlyFileNamed: 'test.bin'
    do: [:stream |
        stream binary.
        stream size. ---> 4
        stream next. ---> 145
        stream upToEnd. ---> #[250 139 98] ].
{% endhighlight %}

Here is another example which creates a picture in a file named “test.pgm” (portable graymap file format). You can open this file with your favorite drawing program.

{% highlight smalltalk %}
FileStream
    forceNewFileNamed: 'test.pgm'
    do: [:stream |
        stream
            nextPutAll: 'P5'; cr;
            nextPutAll: '4 4'; cr;
            nextPutAll: '255'; cr;
            binary;
            nextPutAll: #(255 0 255 0) asByteArray;
            nextPutAll: #(0 255 0 255) asByteArray;
            nextPutAll: #(255 0 255 0) asByteArray;
            nextPutAll: #(0 255 0 255) asByteArray
    ]
{% endhighlight %}

## Chapter summary
---

Streams offer a better way than collections to incrementally read and write a sequence of elements. There are easy ways to convert back and forth between streams and collections.

* Streams may be either readable, writeable or both readable and writeable.
* To convert a collection to a stream, define a stream “on” a collection, e.g., ReadStream on: (1 to: 1000), or send the messages readStream, etc. to the collection.
* To convert a stream to a collection, send the message contents.
* To concatenate large collections, instead of using the comma operator, it is more efficient to create a stream, append the collections to the stream with nextPutAll:, and extract the result by sending contents.
* File streams are by default character-based. Send binary to explicitly make them binary.
