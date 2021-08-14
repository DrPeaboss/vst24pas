library v02xtest2;

{$mode objfpc}{$H+}

uses
  interfaces,forms,
  vst2entry,v02xtest2main;

  function main(AHost:THostCallback):PAEffect;cdecl;
  begin
    Result:=CreateInstance(AHost,TTestPlugin2).GetAEffect;
  end;

exports
  main name 'VSTPluginMain';

begin
  Application.Initialize;
end.

