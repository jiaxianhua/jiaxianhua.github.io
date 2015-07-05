---
layout: post
title: "那些在学习iOS开发前就应该知道的事（part 2）"
description: ""
category: ios
tags: [ios]
---
{% include JB/setup %}

## 英文原文：[Things I wish I had known before starting iOS development—Part 2](https://medium.com/ios-os-x-development/things-i-wish-i-had-known-before-starting-ios-development-part-2-d696eec65866)

## 翻译原文：[那些在学习iOS开发前就应该知道的事（part 2）](http://www.cocoachina.com/ios/20150611/12111.html)
---

如果你还没读这篇文章的第一部分，请先读完了再来看第二部分。

[那些在学习iOS开发前就应该知道的事（part 1）](http://www.cocoachina.com/ios/20150608/12052.html)：设计师设计出来了一个不错的引导界面，然而当我看到设计稿的时候，我们的app也没几天就要上线了……

![1.gif](/assets/img/ios/start/1.gif)

在第一部分中，我讨论了学习iOS开发的一些基本问题。第二部分将更多地讨论一些实际问题，这些问题都是你在开发iOS应用时会遇到的。

## Debugging
---

毋庸置疑，你在开发应用的过程中一定会遇到很多错误和异常。比如，你会遇到NSInvalidArgumentException，遇到NSInternalInconsistencyException，还会遇到“0xfaded322”这种错误代码。有时候你可以在Stack Overflow或Quora上找到答案，但大部分时候你得自力更生。

Xcode具有断点、视图调试和日志的功能，是debug的不二之选。但毕竟孤掌难鸣，你仍需为它找些得力的帮手。

### Pony Debugger

![21.png](/assets/img/ios/start/21.png)

[square/PonyDebugger](https://github.com/square/PonyDebugger)：PonyDebugger - 使用Chrome Developer Tools，为你的native iOS应用进行远程网络和数据调试。

square开发了这个强大的开源工具。它是一个远程调试工具，类似于客户端库和网关服务器的结合体。它在web浏览器上使用Chrome Developer Tools，对应用的网络流量和数据存储进行调试。作为一个强大的网络调试器，PonyDebugger允许用户实时查看应用的网络请求。它还有另一个酷酷的功能，就是可以远程调试iOS应用的核心数据栈。

### Cocoa Lumberjack

![22.png](/assets/img/ios/start/22.png)

[CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)：CocoaLumberjack - 快速、简单，强大、灵活。它就是CocoaLumberjack， 一款好用的Mac和iOS日志框架。

CocoaLumberjack，它快速、简单，强大、灵活，是一款好用的Mac和iOS日志框架。如果你想找一款强大的日志工具，希望它具有自定义格式化这样的炫酷功能，那CocoaLumberjack就是你的菜。

### Reveal App

![23.png](/assets/img/ios/start/23.png)

[Reveal](http://revealapp.com/)：Reveal - iOS应用的runtime视图调试。

虽然视图调试已经被引入iOS，但Reveal无疑在调试的细节方面更加强大。它是收费应用，但绝对物有所值。它有一些非常好用的功能，如Auto Layout Inspection。

### OHHTTPStubs

![24.png](/assets/img/ios/start/24.png)

[AliSoftware/OHHTTPStubs](https://github.com/AliSoftware/OHHTTPStubs)：OHHTTPStubs - 轻松stub你的网络请求。以虚拟网络数据测试你的应用，可自定义响应时间……

OHHTTPStubs库可以轻松stub你的网络请求。它可以帮助你：

使用伪造的网络数据（stubbed from file）测试你的应用，并模拟慢速网络以检查应用在网络不良情况下的表现。
从设备中获得伪造的网络数据，用于写单元测试。

![25.png](/assets/img/ios/start/25.png)

## 数据存储
---

可能大多数应用都需要将各种任务的数据存储在本地。数据存储是一个复杂的话题。它有很多选择，每个选择都对应一种情况。但我很喜欢Stack Overflow上的一种法则，用它来选择数据存储方式就很不错。

* 若数据完全匹配内存且相对非结构化，则使用plist
* 若数据完全匹配内存且具有树状结构，则使用XML
* 若数据与内存不匹配且具有图形结构，同时应用不需要额外的查询能力，则使用Core Data
* 如果数据与内存不匹配且具有复杂结构，或应用需要关系数据库提供的强查询能力，则使用sqlite
* 如果数据必须保密（例如密码），则使用[keychain](https://developer.apple.com/library/ios/#documentation/security/Conceptual/keychainServConcepts/iPhoneTasks/iPhoneTasks.html)。

下面列了一些数据存储方面的库，或许对大家有帮助。

### FMDB

![26.png](/assets/img/ios/start/26.png)

[ccgus/fmdb](http://cc.cocimg.com/api/uploads/20150610/1433927562572960.png)：fmdb - 围绕SQLite建立的Cocoa / Objective-C的wrapper

如果你在项目中使用SQLite，此wrapper库会使你的工作变简单。

### SSFKeychain

![27.png](/assets/img/ios/start/27.png)

[soffes/sskeychain](https://github.com/soffes/sskeychain)：sskeychain - 简单的Objective-C wrapper，Mac和iOS上的keychain可以使用

要在应用中存储敏感数据，你必须时刻使用keychain。这个库可以简化使用keychain数据存储的过程。

### Magical Record

![28.png](/assets/img/ios/start/28.png)

[MagicalRecord](https://github.com/magicalpanda/MagicalRecord) - 轻松管理Core Data

Core data非常难于管理。而这个库可以让你舒爽地管理Core Data。

## 网络
---

要想让应用更有趣，你免不了要接入一些API。虽然iOS在对网络的支持方面表现良好，譬如它拥有NSURLSession、NSURLConnection和NSJSONSerialization，但我还是推荐你使用下面的库。

### AFNetworking

![29.png](/assets/img/ios/start/29.png)

[AFNetworking](https://github.com/AFNetworking/AFNetworking)：AFNetworking - 令人拍案叫绝的iOS和OS X网络框架。

我认为这是有史以来最好的iOS库之一，它的功能何止炫酷二字。但这些功能中最重要的或许是开发者社区，他们每天都在使用这个库，并为AFNetworking做出贡献。一些iPhone、iPad和Mac上最火的应用都是由AFNetworking提供的支持。

### Restkit

![30.png](/assets/img/ios/start/30.png)

[RestKit/RestKit](https://github.com/RestKit/RestKit)：RestKit是在iOS和OS X上使用和构建RESTful web资源的框架。

Restkit具有精心设计的API，访问和构建RESTful资源的过程如沐春风。如果你用core data进行数据存储、用rest service进行数据读取，那你的最佳选择就是它——与Core Data完美集成的Restkit。

### Alamofire

![31.png](/assets/img/ios/start/31.png)

[Alamofire/Alamofire](https://github.com/Alamofire/Alamofire)：Alamofire - Swift下精致的HTTP Networking。

哟哟切克闹，Swift粉们不要闹，煎饼果子来一套。下面就是为你们准备的东西了。Alamofire是一个精致的网络库，它具有一些Swift专享的强大功能。

你也可以在这里找到很多其他的库。

### vsouza/awesome-ios

![32.png](/assets/img/ios/start/32.png)

awesome-ios——精选的优质iOS生态系统，包括Objective-C和Swift项目。

## 依赖管理
---

我在前面的文章中提到过依赖管理，但重要的问题要说两遍！在这里我还要给它细细讲一发。在项目中，你主要有三种管理依赖的方式。

### CocoaPods

[CocoaPods.org](https://cocoapods.org/)：iOS和Mac项目的依赖管理器。

CocoaPods是Swift和Objective-C Cocoa项目的依赖管理器。它拥有将近一万个库，可以帮助你轻松扩大项目规模。要想管理Ruby Gems这种依赖，实践中最有效的方法就是它了。

谷歌开发者做了一个YouTube视频，解释了为什么要在你的项目中使用CocoaPods。小心笑尿。

点此观看视频：Route 85: [An Introduction to CocoaPods](https://youtu.be/iEAjvNRdZa0?list=PLOU2XLYxmsIKGQekfmV0Qk52qLG5LU2jO)

### Github Submodules

你还可以使用git submodules，在项目中以sub repos形式管理依赖。子模块相对于Cocoapods的优势在于子模块也是sub-repos——这不仅是指git和git GUIs逐渐认可并更加支持它们，也意味着你的依赖可以将git repos和广阔的世界连接起来，而CocoaPods却不能。

但git submodules也有自己的问题：项目中没有那些你所依赖的代码的来源。它只是指向了子模块库。而大多数时间你根本不会去管这个库。

### Carthage

![33.png](/assets/img/ios/start/33.png)

[Carthage/Carthage](https://github.com/Carthage/Carthage)：Carthage - 简单、去中心化的Cocoa依赖管理器。

Carthage旨在为Cocoa应用提供最简单的框架添加方式。Carthage使用xcodebuild建立framework binaries，把整合工作留给了用户。CocoaPods的方法更简单易用，而Carthage的方法更灵活温和。

不幸的是，Carthage也有一个巨大缺陷——仅支持iOS 8及以上版本。

![34.png](/assets/img/ios/start/34.png)

## 测试
---

大多数人一提到应用测试就会呵欠连连。但如果没有测试的话，说不巧哪天你的应用就突然崩溃了。发布应用时，你一定得先进行深度测试，保证用户得到最佳体验。

这里有很多测试框架，它们可以简化测试工作。

### XCTest

XCTest是一个单元测试框架，包含在Xcode中。它支持把单元测试作为项目编译过程的一部分。XCTest与XCode高度整合，因此提供了持续整合支持和覆盖率测试这样的功能。

### KIF

![35.png](/assets/img/ios/start/35.png)

[kif-framework/KIF](https://github.com/kif-framework/KIF)：KIF - Keep It Functional - iOS功能测试框架

KIF是Keep It Functional的缩写。它由Square开源，是一款iOS一体化测试框架。它利用其可访问性——即系统让访问不能显示的应用成为可能，轻松实现了iOS应用的自动化。

### Kiwi

![36.png](/assets/img/ios/start/36.png)

[kiwi-bdd/Kiwi](https://github.com/kiwi-bdd/Kiwi)：Kiwi - BDD for iOS

Kiwi是iOS开发的Behavior Driven Development库。其目标是提供一个安装和使用都非常简单的BDD库。

### Quick

![37.png](/assets/img/ios/start/37.png)

[Quick/Quick](https://github.com/Quick/Quick.git)：Quick - Swift （以及Objective-C）的测试框架。

Quick是Swift和Objective-C的一款行为驱动的开发框架。它由RSpec、Specta和Ginkgo开发。与Quick并肩战斗的是Nimble——一款为测试服务的匹配框架。

我尝试在本文中讲尽量多的技术问题。跳入iOS开发的大坑之后，你就能用得到上我说的这些东西了。关于iOS开发需要说的实在太多，所以在这儿我没有谈到营销之类的话题。如果你想看的话，那就等我的下一篇文章吧！
