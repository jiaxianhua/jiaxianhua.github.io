---
layout: post
title: "unity3D的FingerGestures插件"
description: ""
category: unity
tags: [unity]
---
{% include JB/setup %}

FingerGestures是一个unity3D插件，用来处理用户动作，手势。  译自[FingerGestures官方文档](http://fingergestures.fatalfrog.com/docs/manual:start) 

## 目录
---

* [FingerGestures包结构][]
* [FingerGestures例子列表][]
* [设置场景][]
* [教程：识别一个轻敲手势][]
* [教程：手势识别器][]
* [教程：轻击手势识别器][]
* [教程：拖拽手势识别器][]
* [教程：滑动手势识别器][]
* [教程：长按手势识别器][]
* [教程：缩放手势识别器][]
* [教程：旋转手势识别器][]
* [教程：自定义手势识别器][]
* [教程：识别手势事件][]
* [建议：使用.net代理事件][]


[FingerGestures包结构]: #package_content
[FingerGestures例子列表]: #samples-list
[设置场景]: #setting_up
[教程：识别一个轻敲手势]: #tap_gesture
[教程：手势识别器]: #detecting_gesture
[教程：轻击手势识别器]: #detecting_tap_gesture
[教程：拖拽手势识别器]: #detecting_drag_gesture
[教程：滑动手势识别器]: #detecting_swipe_gesture
[教程：长按手势识别器]: #detecting_long_press_gesture
[教程：缩放手势识别器]: #detecting_pinch_gesture
[教程：旋转手势识别器]: #detecting_twist_gesture
[教程：自定义手势识别器]: #detecting_custom_gesture
[教程：识别手势事件]: #detecting_finger_event
[建议：使用.net代理事件]: #using_net_event

-----

## fingerGestures包结构 <a name="package_content"></a>     
---

<table class="table  table-striped  table-condensed">
	<tbody><tr class="row0">
		<th class="col0"> 路径，相对Assets/Plugin/… </th><th class="col1"> 描述 </th>
	</tr>
	<tr class="row1">
		<td class="col0"> FingerGestures/ </td><td class="col1"> 插件的根目录 </td>
	</tr>
	<tr class="row2">
		<td class="col0"> FingerGestures/Prefabs </td><td class="col1"> 可以直接拖放到场景中的预设资源(prefabs)</td>
	</tr>
	<tr class="row3">
		<td class="col0"> FingerGestures/Scripts </td><td class="col1"> 核心脚本和组件</td>
	</tr>
	<tr class="row4">
		<td class="col0"> FingerGestures/Scripts/Gesture Recognizers </td><td class="col1"> 每个手势识别 
的脚本</td>
	</tr>
	<tr class="row5">
		<td class="col0"> FingerGestures/Scripts/Finger Event Detectors </td><td class="col1"> 每个触摸事件检测器的脚本 </td>
	</tr>
	<tr class="row6">
		<td class="col0"> FingerGestures/Scripts/Components </td><td class="col1"> 手势识别和触摸事件所需要添加的额外组件</td>
	</tr>
	<tr class="row7">
		<td class="col0"> FingerGestures/Toolbox </td><td class="col1"> FingerGestures 自带的工具箱脚本 </td>
	</tr>
	<tr class="row8">
		<td class="col0"> FingerGestures/Samples.unitypackage </td><td class="col1"> 所有例子的子包 </td>
	</tr>
	<tr class="row9">
		<td class="col0"> FingerGestures/PlayMaker Actions.unitypackage </td><td class="col1"> FingerGestures对PlayMaker扩展的插件 </td>
	</tr>
	<tr class="row10">
		<td class="col0"> Editor/FingerGestures </td><td class="col1"> FingerGestures对编辑器的扩展 </td>
	</tr>
</tbody></table>  
	
## FingerGestures例子列表 <a name="samples-list"></a>   
---


* __Finger Event(鼠标或手指事件)__   
	__FingerEventsPart1:__ 展示如何通过不同的检测器（ FingerEventDetectors ）去检测鼠标或者手指的上（down）、下（up），按下不移动（stationary，悬停（hover） 事件。  
	__FingerEventsPart2:__ 展示如何识别不同鼠标或者手指动作（FingerMotionDetector）。  

* __Gestures（手势）__  	
	__BasicGestures:__ 识别单击（react to tap），双击（double tap），拖动（drag），长按（long——press），滑动（swipe）等基础手势。  
	__PinchAndTwist:__ 两个或多个手指同时在触摸屏上挤压（pinch）或扭转（twist）时，触发手势的事件。（PS：通常都是用来缩放或旋转）  
	__PointCloudGestures:__ 示范如何识别一个点云（point cloud）手势。（PS：通常是指用用户画的图案作为识别）   

* __Toolbox（工具箱）__  
	__Camera（放入摄像机的脚本）：__  
	Toolbox-DragView:  展示使用`TBDragView `脚本，实现拖动视角。  
	Toolbox-Orbit:     展示使用`TBOrbit`脚本，实现围绕目标旋转视角。  
	Toolbox-Pan:       展示使用`TBPan`脚本，实现以自身为轴旋转视角。  
	Toolbox-PinchZoom: 展示使用`TBPinchZoom`脚本，实现变焦。  

	__Object-Based（放入普通场景对象的脚本）：__  
	Toolbox-Drag:          展示使用`TBDrag `脚本，实现简单的物体拖动  
	Toolbox-Hover:         展示使用`TBHoverChangeMaterial ` 和 `TBHoverChangeScale `脚本，实现当鼠标或者手指悬停在物体上时候的响应。（PS：类似鼠标放到图标上，图标发亮的效果）   
	Toolbox-PinchToScale   展示使用`TBPinchToScale `脚本，实现缩放物体  
	Toolbox-TwistToRotate: 展示使用`TBTwistToRotate `脚本，实现旋转物体    
 	

## 设置场景   <a name="setting_up"></a> 
---

需要在场景中实例化一个FingerGesture组件才可使用。 FingerGesture在项目中的作用是管理用户输入和识别手势和鼠标或手指事件。   
有两种添加方式，一是直接把Plugins\FingerGestures\Prefabs下的`FingerGestures` prefab文件拖入场景中。二是可以创建一个空物件，然后把`FingerGestures`组件添加进去。

![1](/assets/img/unity/fingergestures/scene_setup_fingergestures.png)  

使用` Make Persistent `标志可以让使FingerGestures 单例在跨场景后一直有效，所以只要保证它在第一个场景设置就足够。  
   
## 教程：识别一个轻敲手势   <a name="tap_gesture"></a>  
---

该章节会学习到如何识别一个简单的单击动作，然后到特殊物件的单击动作识别，最后到识别一个三个手指的双击动作。  
	
* __初始化__  
	第一步，如上章节设置;  
	第二步，创建一个GameObject 命名为`Gestures` ;  
	第三步，给`Gestures`添加一个`TapRecognizer`组件，并保持默认设置，你可以在项目面板搜索到它或者直接打开Component > FingerGestures > Gestures > Tap menu item。  

	![1](/assets/img/unity/fingergestures/tut_tap1.png)   

	TapRecognizer 是其中一种手势识别器，它用于监控用户输入而且当一个有效的单击动作被识别时候工作。  
	第四步，创建一个新的C# script 叫做 `TapTutorial`并添加到第二步创建的`Gestures`中。  


* __轻敲屏幕__  
	第一步，点击TapGestures组件上的`Copy Event To Clipboard`按钮，它会把TapGesture所需要的时间信号代码copy到黏贴板。  
	第二步，粘贴到`TapTutorial`脚本里，如下:  

{% highlight c# %}
public class TapTutorial : MonoBehaviour
{
    void OnTap( TapGesture gesture ) 
    { 
        /* your code here */ 
    }
} 
{% endhighlight %}

  	`OnTap`函数匹配定义在TapRecognizer 组件内的信息名属性，当识别器要识别一个轻敲手势，它会使用unity3d的`SendMessage API`先向Gestures物件内所有的脚本广播`OnTap`信息，只要TapTutorial绑定在该物件上，它的`OnTap`函数就会被调用到。  
  	出于性能考虑，通常使用.net标准的事件模型代替unity3d的SendMessage API。  
  	第三步，修改`OnTop`函数：

{% highlight c# %}
void OnTap( TapGesture gesture ) 
{
     Debug.Log( "Tap gesture detected at " + gesture.Position + 
            ". It was sent by " + gesture.Recognizer.name );
}  
{% endhighlight %}
    `gesture`参数包含着手势事件数据，在上面的代码，我们主要输出了位置和`TapRecognizer`内工作的事件。你还可以在`gesture`参数内获得更多属性，例如通过`gesture.Fingers`获得鼠标或手指相关的手势列表，还有可以通过`gesture.Selection`获得当前是哪个场景被轻敲 。  
    第四步，可以测试，通过敲不同位置，可以看到debug信息输出。   

## 教程：手势识别器   <a name="detecting_gesture"></a>   
---

在FingerGesture里，用户的手势都由`GestureRecognizers`组件来处理，它是顺序处理被识别匹配的用户动作的。   
	
	
* __找到GestureRecognizers__   	 
	每种手势都有自己的脚本，存放脚本的路径在`Plugins\FingerGestures\Scripts\Gesture Recognizers` 。

	![1](/assets/img/unity/fingergestures/gesture_recognizers_folder.png)   
	你也可以从 `Component > FingerGestures > Gestures menu`里面找到。   
	![1](/assets/img/unity/fingergestures/fingergesture_menu_gestures.png)     

* __基本使用__		
	要识别一个特殊手势，你需要：  
	1、添加对应的`GestureRecognizer `组件到场景中的目标物件。    
	2、配置它的属性。   
	3、监听它的手势事件和对应响应。

	作为手势事件通知的一部分，`GestureRecognizer `传递一个包含相关信息（位置，手指触屏点列表，选择的场景物件，相关的GestureRecognizer等）的事件参数。  

	一个手势识别器有以下监听事件的方式：   
	1、使用标准的.net 委托事件模型，每一个手势识别器都暴露一个.net事件接口。     
	2、使用unity的`SendMessage()`函数 ： 
		手势事件将会被广播到游戏对象里所有的组件。   
		手势事件也可以指向当前相关的场景对象，这时候需要把手势识别器配置到 `Raycaster `组件中才能检测场景对象。   

	这取决于你的选择。.net的事件模型较为高效，unity的`SendMessage()`较为方便。


* __属性__    
	 由同一基类派生出来的各种手势识别器共用一个通用配置和一些函数。例如，我们可以看到`TapRecognizer `和`SwipeRecognizer `组件的配置放置在同一个对象里。  
	 ![1](/assets/img/unity/fingergestures/gesture_recognizer_properties_swipe_tap.png)     
	 设置：    
	 你可以看到，两个组件共用了一部分配置:`fingers setup`,`reset mode`,`event notification settings`,`references to additional components`...     
	 同样，每个手势识别器都有自己独特的配置，例如滑动识别器要设置距离阀值、速度、和偏差。而多点触控可以设置最大持续时间等。

	 事件信息广播：     
	 此处使用`SendMessage() `函数去通知其他系统。你可以使用`Message Name`属性去指定响应的函数名。     
	 通常，`Message Target`会设置你加入的手势识别器组件。但你也可以设置别的对象。   

	 组件：    
	 你可以收到手动指定添加组件。例如：添加一个`ScreenRaycaster `组件让手势识别器获知场景内对象碰撞。并把消息发送到相应的监听器。它允许识别器转发消息到正在有关联的场景对象。    

## 教程：轻击手势识别器   <a name="detecting_tap_gesture"></a>      
---

![1](/assets/img/unity/fingergestures/tap_recognizer.png)     

* __属性__     
	`Required Taps` ：连续轻击的次数。    
	`Max Delay Between Taps`  :两次轻击间最大的时间间隔。（秒）    
	`Movement Tolerance`：连续轻敲时候，和第一次轻击的位置相隔的偏差大小。    
	`Max Duration`：最大可以识别的手指数。   
	
* __事件__     

{% highlight c# %}
void OnTap( TapGesture gesture )
{
    // 轻击的数量
    int taps = gesture.Taps;
}
{% endhighlight %}
 
## 教程：拖拽手势识别器   <a name="detecting_drag_gesture"></a> 
---

![1](/assets/img/unity/fingergestures/drag_recognizer.png)    

* __属性__      
	`Movement Tolerance`：最小的拖动距离才触发识别器。   
	`Apply Same Direction Constraint`：只能用于多点拖拽，打开后，如果所有点不是向同一个方向拖拽，识别器将不会识别。  

* __事件__   
	
{% highlight c# %}
void OnDrag( DragGesture gesture ) 
{
    // 当前识别器阶段 (Started/Updated/Ended)
    ContinuousGesturePhase phase = gesture.Phase;

    // 最后一帧的拖拽/移动数据
    Vector2 deltaMove = gesture.DeltaMove;

    //完整的拖拽数据
    Vector2 totalMove = gesture.TotalMove;
}
{% endhighlight %}

## 教程：滑动手势识别器   <a name="detecting_swipe_gesture"></a>     
---

![1](/assets/img/unity/fingergestures/swipe_recognizer.png)    

* __属性__    
	`Min Distance`: 必须滑动的最小距离。   
	`Max Distance`：允许滑动的最大距离。   
	`Min Velocity`：滑动时候最小速度。  
	`Max Deviation`：允许的最大角度偏差。（度）    

* __事件__     
	
{% highlight c# %}
void OnSwipe( SwipeGesture gesture ) 
{
    // 完整的滑动数据
    Vector2 move = gesture.Move;

    // 滑动的速度
    float velocity = gesture.Velocity;

    // 大概的滑动方向
    FingerGestures.SwipeDirection direction = gesture.Direction;
}
{% endhighlight %}

## 教程：长按手势识别器   <a name="detecting_long_press_gesture"></a>     
---

![1](/assets/img/unity/fingergestures/longpress_recognizer.png)    

* __属性__      
	`Press Duration`：最少长按时间。    
	`Move Tolerance`:长按过程中允许的最大移动偏差。   

* __事件__     
	
{% highlight c# %}
void OnLongPress( LongPressGesture gesture ) 
{
    // 长按持续时间
    float elapsed = gesture.ElapsedTime;
}
{% endhighlight %}

## 教程：缩放手势识别器   <a name="detecting_pinch_gesture"></a>     
---

![1](/assets/img/unity/fingergestures/pinch_recognizer.png)     

* __属性__     
	`Minimum DOT`  ：允许的小向量点积。   
	`Minimum Distance`:两个手指第一次触屏时候允许的最短路径。   

* __事件__   

{% highlight c# %}
void OnPinch( PinchGesture gesture ) 
{
    // 识别器当前状态(Started/Updated/Ended)
    ContinuousGesturePhase phase = gesture.Phase;

    // 当前两个手指的距离
    float gap = gesture.Gap;

    // 当前与上一帧的变动值 
    float delta = gesture.Delta;
}
{% endhighlight %}

## 教程：旋转手势识别器   <a name="detecting_twist_gesture"></a>     
---

![1](/assets/img/unity/fingergestures/twist_recognizer.png)    

* __属性__      
	`Minimum DOT`  ：允许的小向量点积。    
	`Minimum Rotation`:必须的最小自旋角度。  

* __事件__   

{% highlight c# %}
void OnTwist( TwistGesture gesture ) 
{
    // 识别器当前状态 (Started/Updated/Ended)
    ContinuousGesturePhase phase = gesture.Phase;
 
    // 最近一次角度变化（度）
    float delta = gesture.DeltaRotation;
 
    // 总的角度变化（度）  
    float total = gesture.TotalRotation;
}     
{% endhighlight %}

* __桌面仿真__   	
	在桌面环境，你可以通过`left-CTRL`键加上鼠标转轮去调节角度。也可以在`Mouse Input Provider`配置别的按键。   

## 教程：自定义手势识别器 <a name="detecting_custom_gesture"></a>      
---

自从FingerGestures 3.0之后，可以通过`PointCloudRecognizer`识别自定义手势。利用基于[$P recognizer](http://depts.washington.edu/aimgroup/proj/dollar/pdollar.html) 是手势匹配算法实现。现在只支持单手指操作的识别，将来会支持多点自定义手势。      
![1](/assets/img/unity/fingergestures/customgesturesample.png)      
点云识别器会对比用户输入和已经设置好的手势模板，然后会返回最近接近的匹配结果，会返回匹配得分和差距值。   
点云识别器是规模和方向固定不变的，这就意味着它可以识别画得比较大或者小的，也或者是反方向的（李若：从左到右变成从右到左）。     
	
* __点云识别器模板__     
	一个模板包括要识别的手势的数据。是通过一个编辑器编辑的。    
	![1](/assets/img/unity/fingergestures/pointcloud_template_loop.png)      
	创建一个模板需要以下步骤：      
	1：在你的项目编码，右击-> create ->PonitCloud Gesture Template       
	![1](/assets/img/unity/fingergestures/create_pointcloud_template.png)        
	一个新的模板就好添加到项目里面，可以自己重命名。    
	2：选择模板然后在 Inspecrot 面板内点击 Edit。    
	![1](/assets/img/unity/fingergestures/new_pointcloud_template.png)      
	3：然后开始画图案。    
	![1](/assets/img/unity/fingergestures/gesture_template_inspector_updated.png)      

* __使用点云识别器__      
 	第一步：     
 	1：保证场景对象已经设置好了finger gesture的属性。    
 	2：创建一个新的`Gestures`对象。    
 	3：添加一个`PointCloudRecognizer`组件。   
 	![1](/assets/img/unity/fingergestures/pointcloudrecognizer1.png)      
 	以下属性需要特别注意。    
 	`Max Match Distance`：控制识别的精确的程度。数值越低，越精确。    
 	`Sampling Distance`: 连贯动作取样时候，两点间隔的最小距离。越小越精确，但是取样会更多。     
 	`Gesture Templates List`:我们指定的模板列表。  		

 	第二步：    
 	添加刚刚创建的模板拖放到手势模板列表中。    
 	![1](/assets/img/unity/fingergestures/pointcloudrecognizer2.png)      

 	第三步：    
 	1、创建一个c#文件，此处命名为`PointCloudTutorial.cs`。   
 	2、在`PointCloudRecognizer ` 下面创建一个手势对象。   
 	3、编辑c#文件：    

{% highlight c# %}
public class PointCloudTutorial : MonoBehaviour
{
    void OnCustomGesture( PointCloudGesture gesture ) 
    {
        Debug.Log( "Recognized custom gesture: " + gesture.RecognizedTemplate.name + 
            ", match score: " + gesture.MatchScore + 
            ", match distance: " + gesture.MatchDistance );
    }
}
{% endhighlight %}

	手势事件保护下面几个重要属性：    
	`gesture.RecognizedTemplate`： 被认为是最佳匹配的手势模板。    
	`gesture.MatchScore`：一个百分比的值，表示匹配的程度。   
	`gesture.MatchDistance`：一个测量绝对值，表示匹配程度。   
	你还可以使用其他手势的属性。 例如位置和选择对象等属性。    

* __用代码创建模板__     	
你可以使用api字自己的编辑器扩展中在运行时候创建手势模板。  

{% highlight c# %}
void Awake()
{
    PointCloudGestureTemplate triangle = ScriptableObject.CreateInstance<PointCloudGestureTemplate>();
    triangle.name = "Triangle Gesture Template";
    triangle.BeginPoints();
    triangle.AddPoint( 0, 1, 1 );
    triangle.AddPoint( 0, 2, 2 );
    triangle.AddPoint( 0, 3, 1 );
    triangle.AddPoint( 0, 1, 1 );
    triangle.EndPoints();
 
    PointCloudGestureTemplate square = ScriptableObject.CreateInstance<PointCloudGestureTemplate>();
    square.name = "Square Gesture Template";
    square.BeginPoints();
    square.AddPoint( 0, 2, 1 );
    square.AddPoint( 0, 2, 3 );
    square.AddPoint( 0, 4, 3 );
    square.AddPoint( 0, 4, 1 );
    square.AddPoint( 0, 2, 1 );
    square.EndPoints();

    PointCloudRegognizer recognizer = gameObject.AddComponent<PointCloudRegognizer>();
    recognizer.AddTemplate( triangle );
    recognizer.AddTemplate( square );
}
{% endhighlight %}

	第一个参数`AddPoint `是一个笔画的顺序，该api暂时只支持单线笔画的手势。    
	当`EndPoints() `调用时候，手势模板会被格式化，所有的点都会重新绘制成0到1范围的数。   

## 教程：识别手指事件   <a name="detecting_finger_event"></a>     
---

FingerGestures 可以识别向上，向下，悬停，移动，长按等单点输入手势。各种`FingerEventDetector`组件用于识别对应的手指事件，与
`GestureRecognizers `类似，都是通过广播信息去触发。   

* __FingerEventDetector__   
所有的手指事件识别器都派生与一个基础抽象类。通常，每个`FingerEventDetector `实例监控着所有手指事件信号。也可以配置`Finger Index Filter `属性，让其只跟踪特定的手指事件。    
和手势识别器一样，手指事件识别器传递一个事件数据对象，改该对象派生于`FingerEvent `类，包含以下属性：   
<table class="table  table-striped  table-condensed">
	<tbody><tr class="row0">
		<th class="col0"> 属性</th><th class="col1"> 类型</th><th class="col2"> 描述</th>
	</tr>
	<tr class="row1">
		<td class="col0"> Name </td><td class="col1"> string </td><td class="col2"> 消息的名字 </td>
	</tr>
	<tr class="row2">
		<td class="col0"> Detector </td><td class="col1"> FingerEventDetector</td><td class="col2"> 该次事件中的手指事件识别器</td>
	</tr>
	<tr class="row3">
		<td class="col0"> Finger </td><td class="col1"> FingerGestures.Finger</td><td class="col2"> 该次事件中的手指类</td>
	</tr>
	<tr class="row4">
		<td class="col0"> Position</td><td class="col1"> Vector2</td><td class="col2">事件所发生的位置 </td>
	</tr>
	<tr class="row5">
		<td class="col0"> Selection </td><td class="col1"> GameObject </td><td class="col2"> 被选中游戏对象 （依赖`ScreenRaycaster `组件）</td>
	</tr>
	<tr class="row6">
		<td class="col0">Hit</td><td class="col1"> RaycastHit</td><td class="col2"> 光线投射碰撞，由`ScreenRaycaster`提供，在正常显示上非常有用</td>
	</tr>
	
</tbody></table>     

* __FingerUpDetector__    
	
{% highlight c# %}
void OnFingerUp( FingerUpEvent e ) 
{
    //手指已经持续的时间
    float elapsedTime = e.TimeHeldDown;
}	
{% endhighlight %}

* __FingerHoverDetector__

{% highlight c# %}
void OnFingerHover( FingerHoverEvent e ) 
{
    // 检查状态，是进入还是离开.
    if( e.Phase == FingerHoverPhase.Enter )
    {
        Debug.Log( e.Finger + " entered object: " + e.Selection );
    }
   else if( e.Phase == FingerHoverPhase.Exit )
    {
       Debug.Log( e.Finger + " exited object: " + e.Selection );
    }
}  
{% endhighlight %}

* __FingerMotionDetector__     
该识别器能够识别两种事件。   
1、OnFingerMove ：当手指位置距离上一帧位置有发生变化。    
2、OnFingerStationary ：当手指与上一帧位置一样。  
	
{% highlight c# %}
void OnFingerMove( FingerMotionEvent e ) 
{
    float elapsed = e.ElapsedTime;

    if( e.Phase == FingerMotionPhase.Started )
        Debug.Log( e.Finger + " started moving at " + e.Position);
    else if( e.Phase == FingerMotionPhase.Updated )
        Debug.Log( e.Finger + " moving at " + e.Position );
    else if( e.Phase == FingerMotionPhase.Ended )
        Debug.Log( e.Finger + " stopped moving at " + e.Position );
}

void OnFingerStationary( FingerMotionEvent e ) 
{
    float elapsed = e.ElapsedTime;

    if( e.Phase == FingerMotionPhase.Started )
        Debug.Log( e.Finger + " started stationary state at " + e.Position );
    else if( e.Phase == FingerMotionPhase.Updated )
        Debug.Log( e.Finger + " is still stationary at " + e.Position );
    else if( e.Phase == FingerMotionPhase.Ended )
       Debug.Log( e.Finger + " stopped being stationary at " + e.Position );
}
{% endhighlight %}

## 建议：使用.net代理事件   <a name="using_net_event"></a> 
---

当使用unity的`SendMessage()`函数广播事件消息非常方便，但是效率低而且不够.NET代理事件灵活。    
* __Gesture Events__     
每个手势识别器都暴露一个公共的`OnGesture`.NET事件，可以匹配手势事件和手指事件。用法跟用`SendMessage()`一样。   
	
{% highlight c# %}
[RequireComponent( typeof( TapGesture ) )]
public class TapTutorial : MonoBehaviour
{
    void Start()
    {
        // 在对象里面寻找轻击事件识别器
       TapRecognizer tap = GetComponent<TapRecognizer>();

        // 订阅它的.NET事件
       tap.OnGesture += MyTapEventHandler;
   }

    void MyTapEventHandler( TapGesture gesture )
    {
        Debug.Log( "Tap detected at " + gesture.Position );
    }
}	
{% endhighlight %}

有时候你需要停止监听事件。你可以用以下办法：    
	
{% highlight c# %}
tap.OnGesture -= MyTapEventHandler; 
{% endhighlight %}

注意停止监听事件时候相关对象的生命周期，有可能会导致内存泄露，这是.NET代理事件的陷阱。   
	
另外一种方法是，`FingerGestures`单例暴露一个全局的`OnGestureEvent`钩子，可以监听到任何手势事件。   

{% highlight c# %}
void Start()
{
    FingerGestures.OnGestureEvent += FingerGestures_OnGestureEvent;
}

void FingerGestures_OnGestureEvent( Gesture gesture )
{
    Debug.Log( gesture.Recognizer.name + " fired its gesture event" );

    if( gesture is TapGesture )
       Debug.Log( "Tapped: " + ((TapGesture)gesture).Taps );
}
{% endhighlight %}

* __Finger Event__     
跟上面类似用法。  

{% highlight c# %}
FingerUpDetector.OnFingerUp( FingerUpEvent e )
FingerDownDetector.OnFingerDown( FingerDownEvent e )
FingerHoverDetector.OnFingerHover( FingerHoverEvent e )
FingerMotionDetector.OnFingerMove( FingerMotionEvent e )
FingerMotionDetector.OnFingerStationary( FingerMotionEvent e )
FingerGestures.OnFingerEvent( FingerEvent e ) 
{% endhighlight %}

原文：<http://dp0304.com/unity3d/2013/07/28/fingergestures/>
