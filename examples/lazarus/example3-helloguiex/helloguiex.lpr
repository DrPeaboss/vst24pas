library helloguiex;

{$mode objfpc}{$H+}

uses
  interfaces,
  Forms,
  vst24pas.utils,
  umain;

  function main(vsthost:TVstHostCallback):PEffect;cdecl;export;
  begin
    Result:=DoVSTPluginMain(TNewGain,vsthost,3,1);
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
  Application.Initialize;
end.

