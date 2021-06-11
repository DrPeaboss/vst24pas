library example2;

{$R *.res}

uses
  vst24pas.core,
  vst24pas.utils,
  umain in 'umain.pas';

function VSTPluginMain(VstHost: TVSTHostCallback): PAEffect; cdecl; export;
begin
  Result := DoVSTPluginMain(TMyPlugin, VstHost, 0, 1);
end;

exports
  VSTPluginMain name 'VSTPluginMain',
  VSTPluginMain name 'main';

begin
end.

