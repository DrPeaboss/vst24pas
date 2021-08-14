library v02xtest1;

{$mode objfpc}{$H+}

uses
  vst2entry,umain;

  function main(host:THostCallback):PAEffect;cdecl;
  begin
    Result:=CreateInstance(host,TMyPlugin).GetAEffect;
  end;

exports
  main name 'VSTPluginMain',
  main name 'main';

begin
end.

