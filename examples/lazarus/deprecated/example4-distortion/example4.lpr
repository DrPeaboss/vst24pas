library example4;

{$mode objfpc}{$H+}

uses
  interfaces,
  Forms,
  vst24pas.core,
  vst24pas.utils,
  umain;

  function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
  begin
    Result := DoVSTPluginMain(TDistortion, VstHost, 0, 1);
  end;

exports
  VSTPluginMain Name 'VSTPluginMain',
  VSTPluginMain Name 'main';

begin
  Application.Initialize;
end.
