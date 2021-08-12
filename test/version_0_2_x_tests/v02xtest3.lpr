library v02xtest3;

{$mode objfpc}{$H+}

uses
  vst2intf, v02xtest3main, interfaces, forms;

  function main(host:THostCallback):PAEffect;
  begin
    Result:=TV02XTest3.Create(host).Base.GetEffect;
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
  Application.Initialize;
end.

