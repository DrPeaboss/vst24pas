library example5;

{$mode objfpc}{$H+}

uses
  interfaces,
  forms,
  vst24pas.core,
  vst24pas.utils,
  umain;

  function main(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
  begin
    Result := DoVSTPluginMain(TFilter, VstHost, 0, 2);
  end;

exports
  main Name 'VSTPluginMain',
  main Name 'main';

begin
  Application.Initialize;
end.

