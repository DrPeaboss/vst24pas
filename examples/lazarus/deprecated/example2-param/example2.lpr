library example2;

{$mode objfpc}{$H+}

uses
  vst24pas.core,
  vst24pas.utils,
  umain;

  function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
  begin
    Result := DoVSTPluginMain(TMyPlugin, VstHost, 0, 1);
  end;

exports
  VSTPluginMain Name 'VSTPluginMain',
  VSTPluginMain Name 'main';

begin
end.
