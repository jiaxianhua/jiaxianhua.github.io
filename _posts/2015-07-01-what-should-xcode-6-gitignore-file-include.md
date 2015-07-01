---
layout: post
title: "What should Xcode 6 gitignore file include?"
description: ""
category: xcode
tags: [xcode, stackoverflow]
---
{% include JB/setup %}

<http://stackoverflow.com/questions/18939421/what-should-xcode-6-gitignore-file-include#>

## new

Another answer is that there's a website called [gitignore.io](https://www.gitignore.io) , which generates the files based on the .gitignore templates from <https://github.com/github/gitignore>.


## old

The easiest answer is that mine looks like this:

{% highlight bash %}
# Xcode
.DS_Store
build/
*.pbxuser
!default.pbxuser
*.mode1v3
!default.mode1v3
*.mode2v3
!default.mode2v3
*.perspectivev3
!default.perspectivev3
*.xcworkspace
!default.xcworkspace
xcuserdata
profile
*.moved-aside
DerivedData
.idea/
# Pods - for those of you who use CocoaPods
Pods
{% endhighlight %}

which I believe is the same .gitignore that GitHub sets up with all their repositories by default.

