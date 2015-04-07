---
layout: post
title: "Jekyll Code snippet highlighting"
description: ""
category: jekyll
tags: [jekyll, pygments, highlighting ]
---
{% include JB/setup %}

<http://jekyllrb.com/docs/templates/>

## Mac OS X
---

`pip install pygments`

> /Users/jiaxianhua/.virtualenvs/jekyll/lib/python2.7/site-packages/pip/_vendor/requests/packages/urllib3/util/ssl_.py:79: InsecurePlatformWarning: A true SSLContext object is not available. This prevents urllib3 from configuring SSL appropriately and may cause certain SSL connections to fail. For more information, see https://urllib3.readthedocs.org/en/latest/security.html#insecureplatformwarning.
>
>  InsecurePlatformWarning
>
> Collecting pygments
>
> /Users/jiaxianhua/.virtualenvs/jekyll/lib/python2.7/site-packages/pip/_vendor/requests/packages/urllib3/util/ssl_.py:79: InsecurePlatformWarning: A true SSLContext object is not available. This prevents urllib3 from configuring SSL appropriately and may cause certain SSL connections to fail. For more information, see https://urllib3.readthedocs.org/en/latest/security.html#insecureplatformwarning.
>
> InsecurePlatformWarning
>
> Downloading Pygments-2.0.2-py2-none-any.whl (672kB)
>
>    100% |████████████████████████████████| 675kB 226kB/s
>
> Installing collected packages: pygments
>
> Successfully installed pygments-2.0.2
>

`$ pygmentize -S default -f html > assets/themes/bootstrap-3/css/
pygments.css`

`vim _includes/themes/bootstrap-3/default.html`

>     <!-- Custom styles -->
>     <link href="{{ ASSET_PATH }}/css/style.css?body=1" rel="stylesheet" type="text/css" media="all">
>     <!-- highlighting -->
>     <link href="{{ ASSET_PATH }}/css/pygments.css" rel="stylesheet">

## Code snippet highlightingPermalink
---

Jekyll has built in support for syntax highlighting of over 100 languages thanks to Pygments. To use Pygments, you must have Python installed on your system and set highlighter to pygments in your site’s configuration file.

Alternatively, you can use Rouge to highlight your code snippets. It doesn’t support as many languages as Pygments, however it should suit most use cases. Also, since Rouge is written in pure Ruby, you don’t need Python on your system!

To render a code block with syntax highlighting, surround your code as follows:


{% highlight ruby %}
def foo
  puts 'foo'
end
{% endhighlight %}

The argument to the highlight tag (ruby in the example above) is the language identifier. To find the appropriate identifier to use for the language you want to highlight, look for the “short name” on the Pygments’ Lexers page or the Rouge wiki.
Line numbersPermalink

There is a second argument to highlight called linenos that is optional. Including the linenos argument will force the highlighted code to include line numbers. For instance, the following code block would include line numbers next to each line:

> {% highlight ruby linenos %}
def foo
  puts 'foo'
end
{% endhighlight %}
>

Stylesheets for syntax highlightingPermalink

In order for the highlighting to show up, you’ll need to include a highlighting stylesheet. For an example stylesheet you can look at syntax.css. These are the same styles as used by GitHub and you are free to use them for your own site. If you use linenos, you might want to include an additional CSS class definition for the .lineno class in syntax.css to distinguish the line numbers from the highlighted code.
