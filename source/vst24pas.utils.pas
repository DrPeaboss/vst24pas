{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst24pas.utils
// Description : Some utils
// Created by  : PeaZomboss, 2021/5
-------------------------------------------------------------------------------}
unit vst24pas.utils;

{$I vst24pas.inc}

interface

uses
  vst24pas.Core, vst24pas.Base, vst24pas.gui;

type
  { VSTPluginMain function for Windows OS}
  TVSTPluginMainDll = function(VstHost: TVstHostCallback): PAEffect; cdecl;

  TVSTPluginClass = class of TVstPlugin;

  PAEffect = ^TAEffect; // Redefine for VSTPluginMain
  TVstHostCallback = AudioMasterCallback; // Redefine for VSTPluginMain

{ Used for VSTPluginMain, here is an example:
--------------------------------------------------------------------------------
function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
begin
  Result := DoVSTPluginMain(TMyPlugin, VstHost, 0, 0);
end;
-------------------------------------------------------------------------------}
function DoVSTPluginMain(PluginClass: TVSTPluginClass; VstHost: TVstHostCallback;
  NumPrograms, NumParams: Int32): PAEffect;

// Convert amplitude to decibel, usually amplitude is in range [0,1]
function VstAmp2dB(Amp: double): double; inline;
// Convert decibel to amplitude, usually decibel is in range (-∞,0]
function VstdB2Amp(dB: double): double; inline;

// Use 4 charactors to make a unique ID
function MakeUniqueID(a,b,c,d:AnsiChar):Int32;inline;
// Used for VstPluginClass to quickly init in constructor
function VstPluginit(Plugin:TVstPlugin;UniqueID:Int32):boolean;inline;overload;
function VstPluginit(Plugin:TVstPlugin;UniqueID:Int32;GUIClass:TVstGUIClass):boolean;inline;overload;

var
  gPlugin:TVstPlugin; // Plugin global variable for GUI, must call VstPluginit first

implementation

uses
  Math;

function DoVSTPluginMain(PluginClass: TVSTPluginClass; VstHost: TVstHostCallback;
  NumPrograms, NumParams: Int32): PAEffect;
var
  plugin: TVstPlugin;
begin
  plugin := PluginClass.Create(VstHost, NumPrograms, NumParams);
  Result := plugin.GetAEffect;
end;

function VstAmp2dB(Amp: double): double;
begin
  Result := 20 * Log10(Amp);
end;

function VstdB2Amp(dB: double): double;
begin
  // Result := Power(10, dB * 0.05);
  // Power(10,db*0.05)=exp(db*0.05*ln(10))=below
  Result:=exp(db*0.1151292546497{0228420089957273422});
end;

function MakeUniqueID(a, b, c, d: AnsiChar): Int32;
begin
  Result:=CCONST(a,b,c,d);
end;

function VstPluginit(Plugin: TVstPlugin; UniqueID: Int32): boolean;
begin
  if not Assigned(Plugin) then exit(False);
  gPlugin:=Plugin;
  Plugin.SetUniqueID(UniqueID);
  Result:=True;
end;

function VstPluginit(Plugin: TVstPlugin; UniqueID: Int32; GUIClass: TVstGUIClass): boolean;
var
  Gui:TVstGUI;
begin
  if not VstPluginit(Plugin,UniqueID) then exit(False);
  Gui:=GUIClass.Create(nil);
  Plugin.SetEditor(TGuiEditor.Create(Gui,Plugin));
  Result:=true;
end;

end.
