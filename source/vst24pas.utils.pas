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
  vst24pas.Core, vst24pas.Base;

type
  { VSTPluginMain function for Windows OS}
  TVSTPluginMainDll = function(VstHost: TVstHostCallback): PAEffect; cdecl;

  TVSTPluginClass = class of TVstPlugin;

{ Used for VSTPluginMain, here is an example:
--------------------------------------------------------------------------------
function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
begin
  Result := DoVSTPluginMain(TMyPlugin, VstHost, 0, 0);
end;
-------------------------------------------------------------------------------}
function DoVSTPluginMain(PluginClass: TVSTPluginClass; VstHost: TVstHostCallback;
  NumPrograms, NumParams: Int32): PAEffect;

function VstAmp2dB(Amp: double): double; inline;
function VstdB2Amp(dB: double): double; inline;

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

end.
