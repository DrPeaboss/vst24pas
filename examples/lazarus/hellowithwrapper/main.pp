unit main;

{$mode objfpc}{$H+}

interface

uses
  vst2intf,vst2wrapper;

type

  { TGain }

  TGain = class(TVst2Wrapper)
  private
    FParam0:Single;
  protected
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
    function GetParameter(index:Int32):Single; override;
    procedure SetParameter(index:Int32; value:Single);override;
    procedure GetEffectName(text:PAnsiChar);override;
    procedure GetVendorString(text:PAnsiChar);override;
    procedure GetProductString(text:PAnsiChar);override;
    function GetVendorVersion:Int32;override;
    procedure GetParamName(index:Int32;text:PAnsiChar);override;
    procedure GetParamLabel(index:Int32;text:PAnsiChar);override;
    procedure GetParamDisplay(index:Int32;text:PAnsiChar);override;
  public
    constructor Create(AHost:THostCallback;NumPresets,NumParams:Int32);override;
  end;

implementation

uses
  vst2utils;

{ TGain }

procedure TGain.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
  TheGain:Single;
begin
  TheGain:=FParam0*2;
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=inputs[0,i]*TheGain;
    outputs[1,i]:=inputs[1,i]*TheGain;
  end;
end;

function TGain.GetParameter(index:Int32):Single;
begin
  Result:=0;
  if index=0 then Result:=FParam0;
end;

procedure TGain.SetParameter(index:Int32;value:Single);
begin
  if index=0 then FParam0:=value;
end;

procedure TGain.GetEffectName(text:PAnsiChar);
begin
  VstStrncpy(text,'vst24pas example: hellowithwrapper',63);
end;

procedure TGain.GetVendorString(text:PAnsiChar);
begin
  VstStrncpy(text,'PeaZomboss',15);
end;

procedure TGain.GetProductString(text:PAnsiChar);
begin
  VstStrncpy(text,'vst24pas project',31);
end;

function TGain.GetVendorVersion:Int32;
begin
  Result:=$00002000;
end;

procedure TGain.GetParamName(index:Int32;text:PAnsiChar);
begin
  if index=0 then VstStrncpy(text,'Gain',7);
end;

procedure TGain.GetParamLabel(index:Int32;text:PAnsiChar);
begin
  if index=0 then VstStrncpy(text,'dB',7);
end;

procedure TGain.GetParamDisplay(index:Int32;text:PAnsiChar);
begin
  if index=0 then VstStrncpy(text,VstAmp2dBString(FParam0*2),7);
end;

constructor TGain.Create(AHost:THostCallback;NumPresets,NumParams:Int32);
begin
  inherited Create(AHost,NumPresets,NumParams);
  SetUniqueID(MakeLong('PZE1'));
  SetVersion($00000200);
  FParam0:=0.5;
end;

end.

