---
layout: post
title: "iOS证书说明和发布内购流程整理"
description: ""
category: ios
tags: [ios, Certificate]
---
{% include JB/setup %}

## 摘要
---
网上关于苹果证书生成和设置的教程比较多，但大多数只是讲了相关流程和步骤，有的也只是简要进行了相关说明，总之介绍的不够详细，自己最初在接触的时候也是比较困惑，下面进行总结

### 关于证书
---

1. 首先通过钥匙串访问——证书助理——从证书颁发机构请求证书——填写证书信息（邮箱，常用名称，存储到磁盘）——存储为（自定义名称.certSigningReuqest，简称CSR文件，只是为了提交到苹果开发者账号中，然后就没用了）到本地

2. 苹果开发者账号中，创建证书（Development和Production）——上传CSR文件——下载证书运行 （ xxx.cer文件）

> 注意：只有在当前电脑中生成本地生成证书，上传到苹果开发账号，然后下载cer文件运行后，钥匙串中才有证书以及对应的秘钥

![Certificate-1](/assets/img/ios/Certificate-1.jpg)
![Certificate-2](/assets/img/ios/Certificate-2.jpg)
![Certificate-3](/assets/img/ios/Certificate-3.png)
![Certificate-4](/assets/img/ios/Certificate-4.png)
![Certificate-5](/assets/img/ios/Certificate-5.png)

如果开发者B，登录开发者账号，下载证书（cer文件）运行，**只有证书没有秘钥，是不能正常使用的**

![Certificate-6](/assets/img/ios/Certificate-6.jpg)

**所以如果有新同事加入到开发组的时候，应该从本地钥匙串中选择证书，导出p12文件（包含证书和秘钥）给同事。另外可以给同事一份Provisioning Profiles文件（配置文件），用于本地开发识别测试设备**

导出p12文件：钥匙串——选择证书——右键导出——存储为——设置p12文件密码

(发给同事后，双击p12文件，输入密码，本地安装证书成功)

需要强调一点，证书和项目关系其实并不大，证书一般有效期只有一年，当证书过期后，只需要重新生成一份证书，上传到开发者账号就行，同时因为原有证书过期，需要重新生成Provisioning Profiles文件。然后给同事们最新的p12文件和Provisioning Profiles文件就行

所以开发者账号中的证书，配置文件是可以放心操作的（比如误删了，或者找不到证书秘钥了）

#### xcode中添加苹果开发者账号

Xcode工具栏——Xcode——Preferences——Accounts—— 左下角 Add Apple ID——输入苹果账号，密码

在项目的target——general——team中可以选择项目对应的开发者账号

![Certificate-7](/assets/img/ios/Certificate-7.png)

（当bulid的新设备未在开发者账号的devices添加devicetoken的时候，xcode会进行提示无法识别设备，可以在xcode中fix issue，xcode会自动在开发者账号中，创建一个新的针对这个设备的Provisioning Profiles配置文件，然后安装到本地，唯一的不好就是开发者账号的配置文件下会有很多零散的配置文件）

### 关于App的发布
---

修改项目的version，以及项目的版本debug为release

**（debug改为release后需要进行测试，一些第三方类库可能release版会有一些不兼容）**

Product——Scheme——Edit Scheme 修改 Run/Test/Analyze/Archive 的build configuration  （发布的时候，只需要Archive就可以了）

![Certificate-8](/assets/img/ios/Certificate-8.jpg)

苹果开发者中心——iTunes Connect——我的APP——创建/选择应用——填写基本修改/添加新版本（构建版本）

#### 发布验证

Product——Desination——选择iOS Device 

Product——Archive——右侧点击Validate——选择证书——validate——等待——Validate Successful——右侧点击Submit to App Store（提交构建版本）——Submission Successful

![Certificate-9](/assets/img/ios/Certificate-9.jpg)
![Certificate-10](/assets/img/ios/Certificate-10.jpg)
![Certificate-11](/assets/img/ios/Certificate-11.jpg)

苹果开发者中心——iTunes Connect——我的APP——选择应用——提交构建版本成功——选择自动发布/手动发布——提交审核

等待审核

#### 关于苹果内购

传送门：[专题：iOS应用内置付费IAP](http://mobile.51cto.com/iphone-410162.htm)

![Certificate-12](/assets/img/ios/Certificate-12.png)

购买流程：

1. 程序向服务器发送请求，获得一份产品列表。

2. 服务器返回包含产品标识符的列表。

3. 程序向App Store发送请求，得到产品的信息。

4. App Store返回产品信息。

5. 程序把返回的产品信息显示给用户（App的store界面）

6. 用户选择某个产品

7. 程序向App Store发送支付请求

8. App Store处理支付请求并返回交易完成信息。

9. 程序从信息中获得数据，并发送至服务器。

10. 服务器纪录数据，并进行审(我们的)查。

11. 服务器将数据发给App Store来验证该交易的有效性。

12. App Store对收到的数据进行解析，返回该数据和说明其是否有效的标识。

13. 服务器读取返回的数据，确定用户购买的内容。

14. 服务器将购买的内容传递给程序。


**因为涉及到ipa破解软件，存在假购买情况，需要后台进行订单验证**

下图自己画的基于ios7的购买流程图（前后台接口调用验证等）

![Certificate-13](/assets/img/ios/Certificate-13.png)
