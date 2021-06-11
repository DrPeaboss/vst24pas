unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.core, vst24pas.base;

type

  { TMyPlugin }

  TMyPlugin = class(TVstPlugin)
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(inputs, outputs: PPSingle; SampleFrames: Int32); override;
  end;

implementation

{ TMyPlugin }

constructor TMyPlugin.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetUniqueID(CCONST('P', 'Z', 'e', '1')); // As unique as you can
end;

procedure TMyPlugin.ProcessReplacing(inputs, outputs: PPSingle; SampleFrames: Int32);
var
  i: integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    outputs[0, i] := inputs[0, i]; // Left channel
    outputs[1, i] := inputs[1, i]; // Right channel
  end;
end;

end.
