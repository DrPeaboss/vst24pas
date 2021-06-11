library example3;

{$R *.res}

uses
  Forms,
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
  Application.Initialize;
end.

