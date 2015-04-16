---
layout: post
title: "build retroarch nightly for ios"
description: ""
category: retroarch 
tags: [ios, retroarch]
---
{% include JB/setup %}

## Download RetroArch
---

* open <http://buildbot.libretro.com/nightly/ios/> to download the latest RetroArch.

## Download Cores
---

* open <http://buildbot.libretro.com/nightly/ios/latest/> to download latest cores.  

{% highlight bash %}
http://buildbot.libretro.com/nightly/ios/latest/2048_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/3dengine_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/4do_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bluemsx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bsnes_accuracy_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bsnes_balanced_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bsnes_cplusplus98_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bsnes_mercury_accuracy_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/bsnes_performance_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/catsfc_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/desmume_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/dinothawr_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/dosbox_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/emux_chip8_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/emux_gb_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/emux_nes_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/emux_sms_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/fb_alpha_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/fceumm_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/fmsx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/fuse_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/gambatte_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/genesis_plus_gx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/gpsp_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/gw_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/handy_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/hatari_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/lutro_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mame078_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mame_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_gba_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_lynx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_ngp_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_pce_fast_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_pcfx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_psx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_supergrafx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_vb_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mednafen_wswan_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mess_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/meteor_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/mupen64plus_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/nestopia_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/nxengine_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/o2em_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/pcsx_rearmed_interpreter_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/pcsx_rearmed_interpreter_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/pcsx_rearmed_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/picodrive_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/prboom_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/prosystem_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/quicknes_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/scummvm_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/snes9x_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/snes9x_next_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/stella_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/test_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/tgbdual_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/tyrquake_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/ume_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/vba_next_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/vbam_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/vecx_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/virtualjaguar_libretro_ios.dylib.zip
http://buildbot.libretro.com/nightly/ios/latest/yabause_libretro_ios.dylib.zip
{% endhighlight %}

## download iReSign 
---

<https://github.com/maciekish/iReSign>

`git clone https://github.com/maciekish/iReSign`

## move files
---
{% highlight bash %}
$ unzip RetroArch_xxxx-xx-xx_ios.zip
$ unzip *.dylib.zip  
$ mkdir Payload  
$ mv RetroArch.app Payload
$ mv *.dylib Payload/RetroArch.app/modules/
{% endhighlight %}


## code sign cores.
---

{% highlight bash %}
$ cat codesign

#!/bin/bash

CODE_SIGN_IDENTITY="iPhone Developer: Firstname Lastname (XXXXXXXX)"
BUILD_PATH="$PWD"

echo "CODESIGNING DYNAMIC LIBRARIES AND BUILDING IPA"
codesign -fs "$CODE_SIGN_IDENTITY" $BUILD_PATH/Payload/RetroArch.app/modules/\*.dylib

$ ./codesign
{% endhighlight %}

## archieve **RetroArch.ipa**
---

`$ zip -r RetroArch.ipa Payload`

## resign RetroArch.ipa use **iReSign.app**
---

open *iReSign.app* 

1.  Drag your *RetroArch.ipa* file to the first box, or use the browse button.
1.  Drag your *RetroArch.mobileprovision* to the second box, or use the browse button.
1.  Select *modify ID*, input the *com.xxx.RetroArch* to the fourth box.
1.  Select full certificate name from Keychain Access, for example "iPhone Developer: Firstname Lastname (XXXXXXXXXX)" in the bottom box.
1.  Click ReSign! and wait. The resigned file will be saved in the same folder as the original file.

## install RetroArch.ipa use iTunes
---

1. Donnection you iPhone to iTunes.
1. Drag your *RetroArch-resigned.ipa* to iTunes in **App** tab.
1. Click **Synchronization**.

## copy games
---

1. Open **iFunBox** app.
1. Drag game to your **iPhone** -> **shared** -> **RetroArch** folder.

## play game
---

1. oepn **RetroArch** on your iPhone.
1. select *Core Selection* -> *App* -> *modules* -> *xxx.dylib*.(ex. *fcemu_libretro_ios.dylib*).
1. select *Load Content* -> *Home* -> *Document* -> *xxx.zip*.

## set gamepad
---

1. if you have a GamePad. Connect it use Bluetooth.
1. click *top- middle left* to stop the game.
1. select *setting* -> *Input Settings* -> *User 1 B* to config your GamePad.
1. bind other key.
1. if need *setting* -> *Input Settings* -> *Slow motion* -> *Clear Keyboard*.
1. select left top button *resume* to resume game.

## enjoy game
