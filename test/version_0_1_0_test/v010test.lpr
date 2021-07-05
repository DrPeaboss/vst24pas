library v010test;

{$mode objfpc}{$H+}

uses
  interfaces,
  Forms,
  vst24pas.utils,
  umain;

  function main(vsthost:TVstHostCallback):PAEffect;cdecl;export;
  begin
    Result:=DoVSTPluginMain(TNewGain,vsthost,0,1);
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
  Application.Initialize;
end.

