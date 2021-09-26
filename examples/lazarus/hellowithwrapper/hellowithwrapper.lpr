library hellowithwrapper;

{$mode objfpc}{$H+}

uses
  vst2intf,vst2wrapper,main;

  function VSTPluginMain(AHost:THostCallback):PAEffect;cdecl;
  begin
    Result:=DoVst2Main(AHost,TGain,0,1);
  end;

exports
  VSTPluginMain;

begin
end.

