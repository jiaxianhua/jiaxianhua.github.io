---
layout: post
title: "Mac OS X Node.js"
description: ""
category: software 
tags: [Mac OS X, software, Node.js]
---
{% include JB/setup %}

Download Node.js

open <https://nodejs.org/download/> to download Mac OS X Install (.pkg)

install Node.js

open Terminal
{% highlight bash %}
$ node -h
Usage: node [options] [ -e script | script.js ] [arguments] 
       node debug script.js [arguments] 

Options:
  -v, --version        print node's version
  -e, --eval script    evaluate script
  -p, --print          evaluate script and print result
  -i, --interactive    always enter the REPL even if stdin
                       does not appear to be a terminal
  --no-deprecation     silence deprecation warnings
  --throw-deprecation  throw an exception anytime a deprecated function is used
  --trace-deprecation  show stack traces on deprecations
  --v8-options         print v8 command line options
  --max-stack-size=val set max v8 stack size (bytes)
  --icu-data-dir=dir   set ICU data load path to dir
                         (overrides NODE_ICU_DATA)
  --enable-ssl2        enable ssl2
  --enable-ssl3        enable ssl3

Environment variables:
NODE_PATH              ':'-separated list of directories
                       prefixed to the module search path.
NODE_MODULE_CONTEXTS   Set to 1 to load modules in their own
                       global contexts.
NODE_DISABLE_COLORS    Set to 1 to disable colors in the REPL
NODE_ICU_DATA          Data path for ICU (Intl object) data

Documentation can be found at http://nodejs.org/
{% endhighlight %}
