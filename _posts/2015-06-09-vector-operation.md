---
layout: post
title: "向量运算"
description: ""
category: math
tags: [math]
---
{% include JB/setup %}

## 零向量
---

**零向量**：大小为零的向量。

零向量也是惟一一个没有方向的向量。

对于其它任意数m，存在无数多个大小（模）为m的向量。它们构成一个圆。

![vector-operation-1](/assets/img/math/vector-operation-1.png)

对任意正值m，有无数个向量的大小等于它

## 向向量
---

x + (-x) = 0

### 运算法则
---

向量的负向量：只要简单的将向量的每个分量都变负即可。

![vector-operation-2](/assets/img/math/vector-operation-2.png)

向量变负

-[x y] = [-x -y]

-[x y z] = [-x -y -z]

-[x y z w] = [-x -y -z -w]

### 几何解释
---

向量变负，得到一个和原向量大小相等，方向相反的向量。

![vector-operation-3](/assets/img/math/vector-operation-3.png)

## 向量的大小（长度或模）
---

**向量的大小**没有明确表示，需要计算。

**向量的大小**也称作向量的**长度**或**模**。

### 运算法则
---

线性代数中，向量的大小用向量两边加竖线表示，这个标量的**绝对值**在标量两边加单竖线表示类似。

![vector-operation-4](/assets/img/math/vector-operation-4.png)

向量大小

向量的大小就是向量各分量平方和的平方根。

![vector-operation-5](/assets/img/math/vector-operation-5.png)

2D, 3D 向量的大小

### 几何解释
---

![vector-operation-6](/assets/img/math/vector-operation-6.png)

向量大小公式的几何解释

## 标量与向量的乘法
---

标量与向量不能相加，但能相乘。相乘结果将得到一个向量，与原向量平行，但长度不同可方向相反。

### 运算法则

标量与向量的乘法非常直接，将向量的每个分量都与标量相乘即可。

![vector-operation-7](/assets/img/math/vector-operation-7.png)

向量与标量相乘

应用到3D向量，如：

![vector-operation-8](/assets/img/math/vector-operation-8.png)

3D向量与标量相乘

向量也能除以非零标量，效果等同于乘以标量的倒数。

![vector-operation-9](/assets/img/math/vector-operation-9.png)

> 注意

* 标量与向量相乘时，不需要写乘号。将两个量挨着写即表示相乘（常将标量写在左边）。
* 标量与向量的乘法和除法优先级高于加法和减法。
* 标量不能除以向量，并且向量不能除以另一个向量。
* 负向量能被认为是乘法的特殊情况，乘以标量-1。

### 几何解释
---

向量乘以标量*k*的效果是以因子**|k|**缩放向量的长度。

![vector-operation-10](/assets/img/math/vector-operation-10.png)

一个2D向量被多个因子乘的效果

## 标量化向量
---

**单位向量**就是大小为1的向量。

单位向量经常被称作**标准化向量**或更简单地称为**法线**。

### 运算法则
---

对任意非零向量v，都能计算出一个和v方向相同的单位向量v(norm)。这个过程被称作向量的**标准化**，要标准化向量，将向量除以它的大小（模）即可。

![vector-operation-11](/assets/img/math/vector-operation-11.png)

标准化向量

> 零向量不能被标准化。数学上这是不允许的，因为将导致除零。几何上也没有意义，因为零向量没有方向。

### 几何解释
---

2D环境中，如果以原点为尾画一个单位向量，那么向量的头将接触到圆心在原点的单位圆（单位圆的半径为1）。

3D环境中，单位向量将触到单位球。

![vector-operation-12](/assets/img/math/vector-operation-12.png)

2D中的标准化向量

## 向量的加法和减法
---

如果两个向量的维数相同，那么它们能相加，相减。结果向量的维数与原向量相同。向量的回减法的记法和标量加减法的记法相同。

### 运算法则
---

向量加法的运算法则很简单：两个向量相加，将对应分量相加即可。

![vector-operation-13](/assets/img/math/vector-operation-13.png)

两个向量相加

减法解释为加负向量，a-b=a+(-b)。

![vector-operation-14](/assets/img/math/vector-operation-14.png)

两个向量相减

> 注意

* 向量不能与标量或维数不同的向量相加减。
* 和标量加法一样，向量的加法满足交换律，但向量减法不满足交换律。永远有 a+b=b+a，但 a-b=-(b-a)，仅当a=b时，a-b=b-a。

### 几何解释
---

向量a和b相加的几何解释为：

平衡向量，使向量a的头连接向量b的尾，接着从a的尾向b的头画一个向量。这就是向量加法的**三角形法则**。

向量的减法与之类似。

![vector-operation-15](/assets/img/math/vector-operation-15.png)

2D向量加减法的三角形法则

三角形法则能扩展到多个向量的情形中。

![vector-operation-16](/assets/img/math/vector-operation-16.png)

三角形法则扩展到多个向量

向量能被解释为与轴平行的位移序列。

![vector-operation-17](/assets/img/math/vector-operation-17.png)

向量解释为位移序列

### 一个点到另一个点的向量
---

通过三角形法则和向量减法解决。

![vector-operation-18](/assets/img/math/vector-operation-18.png)

用2D向量减法计算从a到b的向量

## 距离公式
---

计算两点之间的距离。

从几何意义上说，两点之间的距离等于从一个点到另一个点的向量的长度。

3D情况：

![vector-operation-19](/assets/img/math/vector-operation-19.png)

a到b的距离等于向量d的长度。

![vector-operation-20](/assets/img/math/vector-operation-20.png)

3D距离公式

2D中的公式更简单。

![vector-operation-21](/assets/img/math/vector-operation-21.png)

2D距离公式

## 向量点乘
---

标题和向量可以相乘，两个向量也可以相乘，有两种不同类型的向量乘法。

首先是**点乘**（**内积**）。

### 运算法则
---

术语**点乘**来自记法*a.b*中的点号。与标量与向量的乘法一样，向量点乘的优先给高于加法和减法。

> 标量乘法和标量与向量的乘法经常可以省略乘号，但在点乘中不能省略。

向量点乘就是对应分量乘积的和，其结果是一个**标量**：

![vector-operation-22](/assets/img/math/vector-operation-22.png)

向量点乘

用连加符号简写为：

![vector-operation-23](/assets/img/math/vector-operation-23.png)

向量点乘的连加记法

应用到2D、3D中，为：

![vector-operation-24](/assets/img/math/vector-operation-24.png)

2D与3D点乘

### 几何解释
---

一般来说，点乘结果描述了两个向量的**相似**程序，点乘结果越大，两向量越相近。几何解释更加直接。

![vector-operation-25](/assets/img/math/vector-operation-25.png)

点乘和向量间的夹角相关

点乘等于向量大小与夹角的cos值的积：

![vector-operation-26](/assets/img/math/vector-operation-26.png)

向量点乘的几何解释

如果a、b是单位向量，就可以以避免除法运算。这种情况下，分母是1，只剩下：

![vector-operation-27](/assets/img/math/vector-operation-27.png)

计算两个向量的夹角

如果不需要的确切值而只需要a和b夹角的类型，可以只取用点乘结果的符号。

![vector-operation-28](/assets/img/math/vector-operation-28.png)

点乘结果的符号可大致确定 的类型

向量大小并不影响点乘结果的符号，所以上表是和a、b无关的。

> 如果a、b中任意一个为0，那么a.b的结果也等于0.

因此，点乘对零向量的解释是，零向量的任意其它向量都垂直。

### 向量投影
---

给定两个向量v和n，能将v分解成两个分量：v(||)和v(_|_)。它们分别平行和垂直于n，并满足v=v(_|_)+v(||)。一般称平行分量v(||)为v在n上的投影。

使用点乘计算投影。

![vector-operation-29](/assets/img/math/vector-operation-29.png)

向量的投影

![vector-operation-30](/assets/img/math/vector-operation-30.png)

v(||)计算公式

只要能够示出v(||)的模，就能够计算出该投影向量的值，利用三角分解求值：

![vector-operation-31](/assets/img/math/vector-operation-31.png)

![vector-operation-32](/assets/img/math/vector-operation-32.png)

向量的投影

当然，如果n是单位向量，除法就不必要了。

知道v(||)，求v(_|_)就很容易了，如下：

![vector-operation-33](/assets/img/math/vector-operation-33.png)


## 向量叉乘
---

另一种向量乘法称作**叉乘**（**叉积**），仅可应用于3D向量。和点乘不一样，点乘得到一个标量并满足交换律，向量叉乘得到一个向量并且不满足交换律。

### 运算法则
---

和点乘一样，术语**叉乘**来自记法*a X b*中的叉号。这里把叉乘号写出来，不能像标量乘法那样省略它。

叉乘公式为：

![vector-operation-34](/assets/img/math/vector-operation-34.png)

叉乘

示例如下：

![vector-operation-35](/assets/img/math/vector-operation-35.png)

叉乘的运算优先级和点乘一样，乘法在加减法之前计算。当点乘和叉乘在一直时，*叉乘优先计算*：a.bXc = a.(bXc)。

因此点乘返回一个标量，同时标量和向量间不能叉乘，所以(a.b)Xc没有定义。

运算a.(bXc)称作**三重积**。

### 几何解释
---

叉乘得到的向量垂直于原来的两个向量。

![vector-operation-36](/assets/img/math/vector-operation-36.png)

向量叉乘

图中，向量a和b在一个平面中，向量aXb指向该平面的正上方，垂直于a和b。

aXb的长度等于向量的大小与向量夹角sin值的积，如下：

![vector-operation-37](/assets/img/math/vector-operation-37.png)

叉乘的长度与向量夹角的sin值有关。

可以看到，||aXb||也等于以a和b为两边的平行四边形的面积。

![vector-operation-38](/assets/img/math/vector-operation-38.png)

叉乘和平等四边形的面积

由经典几何知道可知平行四边形的面积是*bh*，即底和高的乘积。可以验证这一点，通过把一端的三角形*切*下来移到另一边，可构成一个矩形。

![vector-operation-39](/assets/img/math/vector-operation-39.png)

平行四边形面积

设a、b分别为a、b的长度。

![vector-operation-40](/assets/img/math/vector-operation-40.png)

![vector-operation-41](/assets/img/math/vector-operation-41.png)

如果a、b平行或任意一个为0，则aXb=0。叉乘对零向量的解释为：它平行任意其它向量。

> 注意这点和点乘的解释不同
>
> 点乘的解释是和任意其它向量垂直。
>
> 当然，定义零向量平行或垂直于任意向量都是不对的，因为零向量没有方向。

aXb指向哪个方向呢？

通过将a的头与b的尾相接，并检查从a到b是顺时针还是逆时针，能够确定aXb的方向。

在**左手坐标系**中

* 如果a和b呈顺时针，aXb指向自己。
* 如果a和b呈逆时针，aXb远离自己。

在**右手坐标系**中，恰好相反。

* 如果a和b呈顺时针，aXb远离自己。
* 如果a和b呈逆时针，aXb指向自己。

![vector-operation-42](/assets/img/math/vector-operation-42.png)

顺时针方向

![vector-operation-43](/assets/img/math/vector-operation-43.png)

逆时针方向

> 注意，探测顺时针还是逆时针时，必须让a的头与b的尾相接。

叉乘最重要的应用就是创建垂直于平面、三角形或多边形的向量。

## 线性代数公式
---

下表列出了一些有用的公式。

![vector-operation-44](/assets/img/math/vector-operation-44.png)

![vector-operation-45](/assets/img/math/vector-operation-45.png)
