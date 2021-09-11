library hello;

uses
  vst2intf, main;

  function VSTPluginMain(Host:THostCallback):PAEffect;cdecl;
  begin
    Result:=TMyPlugin.Create(Host).GetEffect;
  end;

exports
  VSTPluginMain;

begin
end.

