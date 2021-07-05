unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.new,vst24pas.core;

type

  { TNewGain }

  TNewGain = class(TPluginBase)
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32); override;
    function CustomDisplay(index:int32):string;
  end;

implementation

uses
  ueditor,sysutils;

{ TNewGain }

constructor TNewGain.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  PlugInitBasicInfo('P','Z','n','1',TFormMain);
  PlugInitEffectInfo('v010testPlugin','PeaZomboss','Test');
  PlugInitParamInfo(0,0.5,'Gain','dB',dmCustom);
  OnCustomDisplay := @CustomDisplay;
end;

procedure TNewGain.ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32);
var
  i: Integer;
  v: single;
begin
  v:= 2*ParamInfos[0].Value;
  for i:=0 to SampleFrames-1 do
  begin
    Outputs[0,i]:=Inputs[0,i]*v;
    Outputs[1,i]:=Inputs[1,i]*v;
  end;
end;

function TNewGain.CustomDisplay(index: int32): string;
begin
  if index=0 then
    Result := Format('%.3f',[2*ParamInfos[0].Value]);
end;

end.

