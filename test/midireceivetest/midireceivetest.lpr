library midireceivetest;

{$mode objfpc}{$H+}

uses
  vst2entry, vmain, interfaces, forms;

  function main(AHost:THostCallback):PAEffect;cdecl;
  begin
    Result:=CreateInstance(AHost,TPlug).GetAEffect;
  end;

exports
  main name 'VSTPluginMain';

begin
  Application.Initialize;
end.

