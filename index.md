---
layout: page
title: JiaXianhua
tagline: blog
---
{% include JB/setup %}

# 欢迎来到
---

贾献华个人主页 ： <http://jiaxianhua.com>

技术博客 ： <http://jiaxh.com>

《iOS 开发日志》 官方网站 : <http://iOSDevLog.com>

iOS Dev Log Official Home Page: <http://iOSDevLog.com>

历经九九八十一期的磨难，我的播客 《iOS 开发日志》 终于成功登上了 苹果的 Podcast。

<https://itunes.apple.com/cn/podcast/ios-kai-fa-ri-zhi/id1039229999?mt=2>

喜欢的朋友欢迎订阅噢!

gihub 仓库地址:

<https://github.com/iOSDevLog/iOSDevLog>

---

# 博客
---

<ul class="posts">
  {% for post in site.posts %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></li>
  {% endfor %}
</ul>
