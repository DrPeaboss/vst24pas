library v020test;

{$mode objfpc}{$H+}

uses
  {$ifdef linux}cthreads,{$endif}interfaces,forms,vst2interfaces,vst2pluginbase, umain;

  function main(vsthost:THostCallback):PAEffect;cdecl;export;
  begin
    Result:=DoVSTPluginMain(vsthost,TMyPlugin);
  end;

exports
  main Name 'VSTPluginMain',
  main Name 'main';

begin
  Application.Initialize;
end.

