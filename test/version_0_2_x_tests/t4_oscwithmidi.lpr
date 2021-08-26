library t4_oscwithmidi;

{$mode objfpc}{$H+}

uses
  vst2entry, t4_main, interfaces, forms;

  function main(AHost:THostCallback):PAEffect;cdecl;
  begin
    Result:=CreateInstance(AHost,TEasySynth).GetAEffect;
  end;

exports
  main name 'VSTPluginMain';

begin
  Application.Initialize;
end.

