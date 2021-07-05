library example6;

{$mode objfpc}{$H+}

uses
  interfaces,
  Forms,
  vst24pas.core,
  vst24pas.utils,
  umain;

  function main(VstHost: TVstHostCallback): PAEffect; cdecl; export;
  begin
    Result := DoVSTPluginMain(TWaveForm, VstHost, 0, 0);
  end;

exports
  main Name 'VSTPluginMain',
  main Name 'main';

begin
  Application.Initialize;
end.
