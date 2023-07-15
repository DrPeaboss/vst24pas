# vst24pas

vst2.4 for pascal

The pascal implementation of VST 2.4

You can use it to develop a vst2 plugin with lcl or vcl for win32, win64 and even native linux. Though in linux is not stable for gui.

I recommend not use lcl if you want to build linux plugin.

Limited to my level, there may be bugs.

## install and use

Both fpc and delphi are supported.

Hint:

- FPC 3.2.2 and 3.3.1 are tested. (version 0.1 and 0.2)

- Delphi 10.3.3 and 10.4.2 community edition is tested. (version 0.1 and 0.2)

Please see examples.

### lazarus

Open vst24pas.lpk, then `Use - Add to Project`

In the future, you only need to `New Project - Library`, open `Project Inspector`, find `Add - New Requirement`, search `vst24pas` and add to project.

### delphi

In main menu, click `Project - Options`, in the dialog click `Building - Delphi Compiler` item, you can see `Search path` in the right, find and add `vst24pas\source` in your disk.

Attention to the `Target` option above.

## VST 2 discontinued

See <https://helpcenter.steinberg.de/hc/en-us/articles/4409561018258-VST-2-Discontinued>

In the future, more and more DAW will discontinue VST 2.

So, it's time to use VST 3

## About VST 3

Now you can find the VST 3 API bindings at <https://github.com/DrPeaboss/vst3-pas>
