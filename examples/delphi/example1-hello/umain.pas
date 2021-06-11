unit umain;

interface

uses
  vst24pas.core, vst24pas.base;

type
  TMyPlugin = class(TVstPlugin)
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(Inputs, Outputs: TArrPSingle; SampleFrames: Int32); override;
  end;

implementation

{ TMyPlugin }

constructor TMyPlugin.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetUniqueID(CCONST('P', 'Z', 'D', '1'));
end;

procedure TMyPlugin.ProcessReplacing(Inputs, Outputs: TArrPSingle; SampleFrames: Int32);
var
  i: integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    Outputs[0, i] := Inputs[0, i];
    Outputs[1, i] := Inputs[1, i];
  end;
end;

end.
