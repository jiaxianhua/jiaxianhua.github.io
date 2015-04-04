---
layout: post
title: "git合并多个commit到一个"
description: ""
category: 
tags: [git]
---
{% include JB/setup %}

用`rebase -i`

比如下图的commit 历史，想要把 "Second change" 和 "Third change" 这两个commit合并到一起

![1][1]

那么可以 
`git rebase -i 7a734e9d47895e096313003d6a2e4f697a16e2e3`

> **注意** 7a734e9d47895e096313003d6a2e4f697a16e2e3 是 "Second change" 的**前**一个commit ID。


然后会出现编辑器 (具体什么编辑器看你的配置，在linux下，默认是 vi)列出从 7a734e 后面的所有commit，如下图

![2][2]

因为我们要把 "Second change" 和 "Third change" 合并到一起，所以只需要把 "Third change"前面的那个 pick 改成 squash即可，意思是将 "Third change" 和 它前一个commit （即 "Second change") 合并

修改后应该是这样

![3][3]

然后保存退出编辑器，git 就会执行rebase操作，当他遇到 "Second" 和 "Third" 的时候，会再次启动编辑器告诉你即将合并，让你提供commit message，如下图

![4][4]

默认的包括了两个commit的原始消息，你可以在这里任意修改commit message，比如改成 “Second and Third changes in single commit"，然后保存退出，git就会把这两个commit变成一个新的commit。做完后我们再用git log看一下，就会变成下图

![5][5]


对比原始git log信息，你就可以发现两个commit被合成一个了。


同理，你可以将任意多个commit合并成一个 （第一个commit保持 pick， 后续commit改成 squash即可）

更新到远程仓库  
`git push origin +master`

[1]: /assets/img/git/1.jpg
[2]: /assets/img/git/2.jpg
[3]: /assets/img/git/3.jpg
[4]: /assets/img/git/4.jpg
[5]: /assets/img/git/5.jpg
