unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.core, vst24pas.base, vst24pas.gui;

type

  { TDistortion }

  TDistortion = class(TVstPlugin)
  private
    FThreshold: single;
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32); override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterName(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterLabel(index: Int32; _Label: PAnsiChar); override;
  end;

implementation

uses
  ueditor, Math;

{ TDistortion }

constructor TDistortion.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
var
  Gui: TFormMain;
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetUniqueID(CCONST('P', 'Z', 'e', '4'));
  FThreshold := 0.9;
  Gui := TFormMain.Create(nil);
  SetEditor(TGuiEditor.Create(Gui, self));
  Gui.Editor := FEditor as TGuiEditor;
end;

procedure TDistortion.ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32);
var
  i, n: integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    for n := 0 to 1 do
    begin
      if inputs[n, i] > 0 then
        outputs[n, i] := min(inputs[n, i], FThreshold)
      else
        outputs[n, i] := max(inputs[n, i], -FThreshold);
      outputs[n, i] := outputs[n, i] / FThreshold;
    end;
  end;
end;

function TDistortion.GetEffectName(Name: PAnsiChar): boolean;
begin
  VstStrncpy(Name, 'example4-distortion', 31);
  Result := True;
end;

function TDistortion.GetVendorString(Text: PAnsiChar): boolean;
begin
  VstStrncpy(Text, 'PeaZomboss', 63);
  Result := True;
end;

procedure TDistortion.SetParameter(index: Int32; Value: single);
begin
  if index = 0 then
    FThreshold := Value;
end;

function TDistortion.GetParameter(index: Int32): single;
begin
  if index = 0 then
    Result := FThreshold;
end;

procedure TDistortion.GetParameterName(index: Int32; Text: PAnsiChar);
begin
  if index = 0 then
    VstStrncpy(Text, 'Threshold', 10);
end;

procedure TDistortion.GetParameterDisplay(index: Int32; Text: PAnsiChar);
begin
  if index = 0 then
    dB2String(FThreshold, Text, 7);
end;

procedure TDistortion.GetParameterLabel(index: Int32; _Label: PAnsiChar);
begin
  if index = 0 then
    VstStrncpy(_label, 'dB', 3);
end;

end.
