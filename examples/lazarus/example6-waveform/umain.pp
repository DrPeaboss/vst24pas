unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.core, vst24pas.base;

type
  PBufInfo = ^TBufInfo;

  TBufInfo = record
    CanBuf0:      boolean; // Use double buffer
    I0, I1:       array[0..1, 0..1023] of single;
    SampleFrames: integer;
    Accuracy:     integer;
  end;

  { TWaveForm }

  TWaveForm = class(TVstPlugin)
  private
    FInfo: TBufInfo;
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32); override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    property Info: TBufInfo read FInfo;
    property Accuracy:integer write FInfo.Accuracy;
  end;

implementation

uses
  vst24pas.utils, ueditor;

{ TWaveForm }

constructor TWaveForm.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  VstPluginit(self, CCONST('P', 'Z', 'e', '6'), TFormMain);
  FInfo.Accuracy := 3;
end;

procedure TWaveForm.ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32);
var
  i: integer;
  bufid:byte;
  acc:integer;
begin
  if FInfo.SampleFrames > 512 then
  begin
    FInfo.SampleFrames := 0;
    FInfo.CanBuf0 := not FInfo.CanBuf0; // Change buffer
  end;
  if FInfo.CanBuf0 then bufid:=1 else bufid:=0; // We fill another buffer
  acc:=FInfo.Accuracy;
  for i := 0 to SampleFrames - 1 do
  begin
    Outputs[0, i] := Inputs[0, i];
    Outputs[1, i] := Inputs[1, i];
    if i mod acc = 0 then
    begin
      FInfo.I0[bufid,FInfo.SampleFrames] := Inputs[0, i];
      FInfo.I1[bufid,FInfo.SampleFrames] := Inputs[1, i];
      FInfo.SampleFrames := FInfo.SampleFrames + 1;
    end;
  end;
end;

function TWaveForm.GetVendorString(Text: PAnsiChar): boolean;
begin
  VstStrncpy(Text, 'PeaZomboss', 63);
  Result := True;
end;

function TWaveForm.GetEffectName(Name: PAnsiChar): boolean;
begin
  VstStrncpy(Name, 'example6-waveform', 31);
  Result := True;
end;

end.
