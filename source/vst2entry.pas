{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2entry
// Description : Vst2 entry functions
// Created by  : PeaZomboss, 2021/08

// Use this unit in library uses section
-------------------------------------------------------------------------------}

unit vst2entry;

{$I vst2def.inc}

interface

uses
  vst2intf,vst2plugin;

type
  PAEffect = vst2intf.PAEffect;
  THostCallback = vst2intf.THostCallback;

// Here is an easy way to implement main function
{-----------------------------------------------------------
function main(AHost:THostCallback):PAEffect;cdecl;
begin
  Result:=CreateInstance(AHost,TMyPlugin).GetAEffect;
end;
-----------------------------------------------------------}
// Note TMyPlugin is your plugin class
function CreateInstance(AHost:THostCallback;PluginClass:TVPluginClass):TVPlugin;

implementation

function CreateInstance(AHost:THostCallback;PluginClass:TVPluginClass):TVPlugin;
begin
  Result:=TVPlugin(PluginClass.newinstance).Create(AHost);
end;

end.

