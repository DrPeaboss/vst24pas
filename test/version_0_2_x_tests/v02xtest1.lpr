library v02xtest1;

{$mode objfpc}{$H+}

uses
  vst2intf,umain;

  function main(host:THostCallback):PAEffect;cdecl;
  begin
    Result:=TMyPlugin.Create(host).Base.Effect;
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
end.

