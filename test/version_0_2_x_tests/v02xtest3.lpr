library v02xtest3;

{$mode objfpc}{$H+}

uses
  vst2entry, v02xtest3main, interfaces, forms;

  function main(host:THostCallback):PAEffect;cdecl;
  begin
    Result:=CreateInstance(host,TV02XTest3).GetAEffect;
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
  Application.Initialize;
end.

