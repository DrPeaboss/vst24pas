{ some compiler directives and macro definitions }

{$hints off}
{$warnings off}

{$ifdef FPC}
  {$notes off}
  {$mode delphi}

  {.$modeSwitch advancedRecords} // if use {$mode objfpc} should open

(* Hint:
   If you use {$mode objfpc}, the hosts or the plugin itself will crash.
   The reason is from switch {$Z4} which {$mode objfpc} use it by default.
   But in delphi, neither {$Z1} nor {$Z4}, the hosts or plugin will not crash.
   Tested hosts (native load, 32 bit host load 32 bit plugin, 64 bit too):
     FL Studio:
       FL will crash and flashback to desktop as soon as you load the plugin
       if you use {$mode delphi} and {$Z4}, but if you use {$mode objfpc},
       it only crash when you change the tempo, and FL will show a crash dialog.
     LMMS:
       Plugin can be loaded, but when you play audio, the plugin will crash,
       and LMMS will not crash.
*)
{$endif}

{$ifdef MacOS}
  { Don't use }
  {$ifndef TARGET_API_MAC_CARBON}
    {$define TARGET_API_MAC_CARBON}
  {$endif}
{$endif}

{ Define VST Version }
{$define VST_2_1_EXTENSIONS} // Version 2.1 extensions (08-06-2000)
{$define VST_2_2_EXTENSIONS} // Version 2.2 extensions (08-06-2001)
{$define VST_2_3_EXTENSIONS} // Version 2.3 extensions (20-05-2003)
{$define VST_2_4_EXTENSIONS} // Version 2.4 extensions (01-01-2006)

{ Disable for Hosts to serve Plug-ins below VST 2.4 }
{$ifdef VST_2_4_EXTENSIONS}
  {$define VST_FORCE_DEPRECATED}
{$endif}

{ In FPC must above and close to interface, and directives can't be combined }
{.$H+,$I-,$J-,$R-} // Invalid
{$H+}{$I-}{$J-}{$R-} // Ok
