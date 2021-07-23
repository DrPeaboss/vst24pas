library v02xtest2;

{$mode objfpc}{$H+}

uses
  interfaces,forms,
  vst2intf,v02xtest2main;

  function main(AHost:THostCallback):PAEffect;cdecl;
  begin
    Result:=TTestPlugin2.Create(AHost).Base.Effect;
  end;

exports
  main name 'VSTPluginMain';

begin
  Application.Initialize;
end.

