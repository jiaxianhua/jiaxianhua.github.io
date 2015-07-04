---
layout: post
title: "那些在学习iOS开发前就应该知道的事（part 1）"
description: ""
category: ios
tags: [ios]
---
{% include JB/setup %}

## 英文原文：[Things I wish I had known before starting iOS development—Part 1](https://medium.com/ios-os-x-development/things-i-wish-i-had-known-before-starting-ios-development-part-1-421a05e8447e)

## 翻译原文：[那些在学习iOS开发前就应该知道的事（part 1）](https://medium.com/ios-os-x-development/things-i-wish-i-had-known-before-starting-ios-development-part-1-421a05e8447e)
---

设计师设计出来了一个不错的引导界面，然而当我看到设计稿的时候，我们的app也没几天就要上线了。这个界面模仿了Evernote iOS app的风格。

![1.gif](/assets/img/ios/start/1.gif)

我以迅雷不及掩耳盗铃之势开始在Xcode上编程，用了page view controller和scroll view。在Stack Overflow和Google的帮助下，我用了2天把它完成了。当我把产品给一个同样搞iOS开发的朋友看时，他跟我说，如果我用了[这个开源项目的话](https://github.com/mamaral/Onboard.git)，一个小时就可以搞定一切。

过去这一年我经历了不少类似的事情，这些事情让我成长，让我能够成为一个更好的iOS开发工程师。我想跟大家分享一下我的经验，希望你们可以不再犯我这些错误，一路平坦走向成功。

## 重视基础
---

刚开始学iOS开发的时候，我直接去学习了斯坦福大学的这门课程，非常有用。然而，虽然我从课程中学到了很多，但它并没有教给我多少iOS开发语言的基础知识——当时的iOS开发语言主要是Objective-C。开始写自己的app以后，我发现自己在基础知识方面欠缺很多，这导致我老是弄出来一些bug。

如果你在面向对象编程的语言方面没啥经验的话，我建议你在投身进行iOS开发之前先读一本这方面的经典书籍。我最喜欢的两本是[Big Nerd Ranch Guide for Objective-C](https://www.bignerdranch.com/we-write/objective-c-programming/)和Swift的[The Apple's Guide](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/)。

![2.jpeg](/assets/img/ios/start/2.jpeg)

没错，[Ray Wenderlich](http://www.raywenderlich.com/)上的教程和[Team Treehouse](http://teamtreehouse.com/)上的视频课程能教给你很多,但缺少基础知识的程序员就像无源之水、无本之木，总是长久不了的。

## 良师益友Github
---

我对这个iOS开源社区绝对是真爱。Github上面有无数的优质项目，如AFNetworking, Restkit、JSQMessages，甚是碉堡。你必须学会在这个社区里乘前人栽树之凉。

遇到问题时，先别一根筋急着苦思冥想或创建自己的库，不如先去GitHub或Google找一下有没有相似问题的解决方法。很有可能某个开发者已经写好了一个符合你需求的开源项目。

通过Facebook Groups或Slack chat来开始社区之旅吧！他们很乐意回答你的一切问题。你可以浏览那些好的开源项目，看看大牛们是如何组织代码的，自己也学着点。

这里是GitHub上一些最优质的iOS资源。

[vsouza/awesome-ios](https://github.com/vsouza/awesome-ios0)：awesome-ios——精选的优质iOS生态系统，包括Objective-C和Swift项目。

![3.png](/assets/img/ios/start/3.png)

[matteocrippa/awesome-swift](https://github.com/matteocrippa/awesome-swift)：awesome-swift——收集了很多优质的swift资源。你也可以来贡献自己的力量！

![4.png](/assets/img/ios/start/4.png)

[cjwirth/awesome-ios-ui](https://github.com/cjwirth/awesome-ios-ui)：awesome-ios-ui——优质iOS UI/UX库精选。

![5.png](/assets/img/ios/start/5.png)

如果你想找一些iOS的最佳实践以供自己模仿学习，那请看下面这些。

futurice/ios-good-practices：ios-good-practices——为iOS开发者提供灵感，作者是Futurice的开发者们。

![6.png](/assets/img/ios/start/6.png)

## 了解你的工具
---

多数iOS开发者将Xcode作为开发的首选工具。Xcode有很多强大的特性，如Storyboards、Auto Layout，如果学会用这些的话，相信你的开发效率肯定能上一个台阶。出于某些限制，很多开发者会尽力避免使用Storyboards，但我个人认为，Storyboards是快速布局的有力工具。

学习使用Xcode中的快捷键。虽然看上去使用快捷键没节省几秒钟，但“不积小流，无以成江海“，久而久之省下的时间就多了。以上这些都是我亲测有效的手段，它们在提高开发效率方面给了我很大帮助。

* 使用[Cocoapods](https://cocoapods.org/)来进行依赖管理。你的团队会因此轻松很多。
* 在项目早期就学会使用[持续整合（continuos integration）](https://developer.apple.com/library/ios/documentation/IDEs/Conceptual/xcode_guide-continuous_integration/)，这样就可以避免未来可能出现的冗余工作。
* 使用[Testflight](https://developer.apple.com/testflight/)来分发测试版本。在被苹果收购之后，Testflight变得更加简单易用了，每个人都可以通过它来使用iTunes账号进行测试版本的分发。
* 在app中整合[Crashlytics](https://try.crashlytics.com/)，这样当app崩溃的时候你就可以获取Crash报告了。
* 如果你不想自己弄后端服务器的话，[Parse](https://parse.com/)为我们提供了很好的服务。

## 读一些优质博客和资讯
---

前文已经介绍了一些很棒的iOS开源社区。很多优质的博客是由经验丰富的iOS开发者撰写的，每周都会有一些精彩的内容呈现。我最喜欢的一些包括：

[Cocoa with Love](http://cocoawithlove.com/)：在本博客中，我将专注于使用和管理用户界面中字符串的最佳实践。这是一个相当...

可能是最好的iOS博客。作者是Matt Galaghar。Matt做事情的方式是大师级的。

[iOS Dev Weekly](http://iosdevweekly.com/)：订阅本博客，轻松获得每周最棒的iOS开发网页精选。由Dave Verwer精选并发布...

![7.png](/assets/img/ios/start/7.png)

严格来说这不算个博客，但它每周都有超级超级棒的内容更新。作者是Dave Verwer。

[NSHipster](http://nshipster.com/)：Playgrounds并不是Swift语言本身的特性，它们其实是很棒的展示……

![8.png](/assets/img/ios/start/8.png)

NSHipster对Objective-C和Cocoa进行拾遗的杂志。它由Mattt Thompson每周更新。

[Ray wenderlich](http://www.raywenderlich.com/)：高质量的编程教程：iOS、Android、Mac，还有更多！

![9.png](/assets/img/ios/start/9.png)

Ray Wenderlich的博客（对初学者超有用）

[Custom Controls for iOS and OS X - Cocoa Controls](http://cocoacontrols.com/)：暂无描述

![10.png](/assets/img/ios/start/10.png)

[Peter Steinberger](http://petersteinberger.com/)：苹果在Xcode 6中增加了对NS_DESIGNATED_INITIALIZER 标志的支持，同时也将其添加到了各种各样的框架中……

![11.png](/assets/img/ios/start/11.png)

[Matt Gemmell](http://mattgemmell.com/)：基于我的写作项目进行简要更新。它或许会引起一些人的兴趣……

![12.png](/assets/img/ios/start/12.png)

[Natasha The Robot](http://natashatherobot.com/)：仅仅是又一个WordPress站点

在这些博客中挑选干货，认真阅读，相信你会变成一个更好的iOS开发者。

## 设计也能变轻松
---

很多开发者对于iOS的设计方面谈虎色变。我们总是对设计敬而远之，一股脑儿都扔给设计师去做。但其实，只要稍稍努力，你也可以学会设计自己的app。

现在，设计师和开发者之间的界限日益模糊，因为好多成功的iOS独立开发者包揽了所有app设计、开发和营销工作。我将在下一部分谈到营销方面的事。如果你想设计自己的iOS app的话，不妨学一下Sketch这个工具。Sketch专为应用设计和网页设计而生，简单易上手。

[Bohemian Coding - Sketch 3](http://bohemiancoding.com/sketch/)：Sketch是简单易用的轻量级软件，它强大、灵活而快速。最后……

![13.png](/assets/img/ios/start/13.png)

你可以在网上找到大量的Sketch资源和插件，它们能让你有趣而简单地工作。一旦完成了设计工作，你可以立刻用这个神器将它们整合在一起。

[Free mobile & web prototyping for designers - Marvel](https://marvelapp.com/)：将草图和设计转化为可交互的Web、iPhone、iPad、Android和Apple Watch原型和模板……

![14.png](/assets/img/ios/start/14.png)

下一部分，我将谈一谈在开发自己的app时的必要方法，并且讲一些iOS应用的营销技巧。
