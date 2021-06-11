library example3;

{$mode objfpc}{$H+}

uses
  interfaces,
  Forms,
  vst24pas.core,
  vst24pas.utils,
  uMain;

  function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
  begin
    Result := DoVSTPluginMain(TMyPlugin, VstHost, 0, 1);
  end;

exports
  VSTPluginMain Name 'VSTPluginMain',
  VSTPluginMain Name 'main';

begin
  Application.Initialize;
end.
