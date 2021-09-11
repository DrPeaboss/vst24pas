unit umain;

interface

uses
  vst24pas.core, vst24pas.base, vst24pas.gui;

type
  TMyPlugin = class(TVstPlugin)
  private
    FGain: single;
  public
    constructor Create(VstHost: TVSTHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(const inputs, outputs: TBuffer32; SampleFrames: Int32); override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterName(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterLabel(index: Int32; _label: PAnsiChar); override;
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    function GetProductString(Text: PAnsiChar): boolean; override;
  end;

implementation

uses
  ueditor;

{ TMyPlugin }

constructor TMyPlugin.Create(VstHost: TVSTHostCallback; NumPrograms, NumParams: Int32);
var
  Gui: TFormGain;
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetUniqueID(CCONST('P', 'Z', 'D', '3'));
  FGain := 0.5;
  Gui := TFormGain.Create(nil);
  SetEditor(TGuiEditor.Create(Gui, self));
  Gui.Editor := FEditor as TGuiEditor;
end;

function TMyPlugin.GetEffectName(Name: PAnsiChar): boolean;
begin
  vststrncpy(Name, 'example3-hellogui', 31);
  Result := True;
end;

function TMyPlugin.GetParameter(index: Int32): single;
begin
  case index of
    0: Result := FGain;
    else
      Result := 0;
  end;
end;

procedure TMyPlugin.GetParameterDisplay(index: Int32; Text: PAnsiChar);
begin
  case index of
    0: dB2String(FGain * 2, Text, 7);
    else;
  end;
end;

procedure TMyPlugin.GetParameterLabel(index: Int32; _label: PAnsiChar);
begin
  case index of
    0: vststrncpy(_label, 'dB', 2);
    else;
  end;
end;

procedure TMyPlugin.GetParameterName(index: Int32; Text: PAnsiChar);
begin
  case index of
    0: vststrncpy(Text, 'Gain', 4);
    else;
  end;
end;

function TMyPlugin.GetProductString(Text: PAnsiChar): boolean;
begin
  VstStrncpy(Text, 'vst24pas examples', 63);
  Result := True;
end;

function TMyPlugin.GetVendorString(Text: PAnsiChar): boolean;
begin
  vststrncpy(Text, 'PeaZomboss', 63);
  Result := True;
end;

procedure TMyPlugin.ProcessReplacing(const inputs, outputs: TBuffer32; SampleFrames: Int32);
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

end.

