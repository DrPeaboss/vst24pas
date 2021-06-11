unit uMain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.core, vst24pas.base, vst24pas.gui;

type

  { TMyPlugin }

  TMyPlugin = class(TVstPlugin)
  private
    FGain: single;
  public
    constructor Create(VstHost: TVSTHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(inputs, outputs: PPSingle; SampleFrames: Int32); override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterName(index: Int32; Text: PChar); override;
    procedure GetParameterLabel(index: Int32; _label: PChar); override;
    procedure GetParameterDisplay(index: Int32; Text: PChar); override;
    function GetEffectName(Name: PChar): boolean; override;
    function GetVendorString(Text: PChar): boolean; override;
    function GetProductString(Text: PChar): boolean; override;
  end;

implementation

uses
  uEditor;

constructor TMyPlugin.Create(VstHost: TVSTHostCallback; NumPrograms, NumParams: Int32);
var
  Gui: TFormGain;
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetUniqueID(CCONST('P', 'Z', 'e', '3'));
  FGain := 0.5;
  Gui := TFormGain.Create(nil);
  SetEditor(TGuiEditor.Create(Gui, self));
  Gui.Editor := FEditor as TGuiEditor;
end;

procedure TMyPlugin.ProcessReplacing(inputs, outputs: PPSingle; SampleFrames: Int32);
var
  i: integer;
  Gain: single;
begin
  Gain := 2 * GetParameter(0);
  for i := 0 to SampleFrames - 1 do
  begin
    outputs[0, i] := inputs[0, i] * Gain;
    outputs[1, i] := inputs[1, i] * Gain;
  end;
end;

procedure TMyPlugin.SetParameter(index: Int32; Value: single);
begin
  case index of
    0: FGain := Value;
    else;
  end;
end;

function TMyPlugin.GetParameter(index: Int32): single;
begin
  case index of
    0: Result := FGain;
    else
      Result := 0;
  end;
end;

procedure TMyPlugin.GetParameterName(index: Int32; Text: PChar);
begin
  case index of
    0: vststrncpy(Text, 'Gain', 4);
    else;
  end;
end;

procedure TMyPlugin.GetParameterLabel(index: Int32; _label: PChar);
begin
  case index of
    0: vststrncpy(_label, 'dB', 2);
    else;
  end;
end;

procedure TMyPlugin.GetParameterDisplay(index: Int32; Text: PChar);
begin
  case index of
    0: dB2String(FGain * 2, Text, 7);
    else;
  end;
end;

function TMyPlugin.GetEffectName(Name: PChar): boolean;
begin
  vststrncpy(Name, 'example3-hellogui', 31);
  Result := True;
end;

function TMyPlugin.GetVendorString(Text: PChar): boolean;
begin
  vststrncpy(Text, 'PeaZomboss', 63);
  Result := True;
end;

function TMyPlugin.GetProductString(Text: PChar): boolean;
begin
  VstStrncpy(Text, 'vst24pas examples', 63);
  Result := True;
end;

end.
