unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst24pas.core, vst24pas.base, vst24pas.utils;

type
  { Thanks to http://www.martin-finke.de/blog/articles/audio-plugins-013-filter/ }

  TFilterMode = (fmLowPass=0,fmHighPass,fmBandPass);
  TAttenuationMode = (am12=0,am24);

  { TFilter }

  TFilter = class(TVstPlugin)
  private
    FCutoff:double;
    FResonance:double;
    FFeedbackAmount:double;
    FBuf0,FBuf1,FBuf2,FBuf3:double;
    FMode:TFilterMode;
    FAttenuationMode:TAttenuationMode;
    procedure CalculateFeedbackAmount;inline;
    function ProcessOneValue(inputvalue:double):double;
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    procedure ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32); override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterName(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterLabel(index: Int32; _Label: PAnsiChar); override;
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    property Mode:TFilterMode read FMode write FMode;
    property AttenuationMode:TAttenuationMode read FAttenuationMode write FAttenuationMode;
  end;

implementation

uses
  ueditor;

{ TFilter }

procedure TFilter.CalculateFeedbackAmount;
begin
  FFeedbackAmount := FResonance + FResonance/(1-FCutoff) ;
end;

constructor TFilter.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  VstPluginit(self,CCONST('P','Z','e','5'),TFormFilter);
  FCutoff := 0.99;
  FResonance := 0.01;
end;

procedure TFilter.ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32);
var
  i: Integer;
begin
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=ProcessOneValue(inputs[0,i]);
    outputs[1,i]:=ProcessOneValue(inputs[1,i]);
  end;
end;

procedure TFilter.SetParameter(index: Int32; Value: single);
begin
  case index of
    0:if value<1 then FCutoff := value else FCutoff:=0.9999;
    1:FResonance := value;
  end;
  CalculateFeedbackAmount;
end;

function TFilter.GetParameter(index: Int32): single;
begin
  case index of
    0:Result:=FCutoff;
    1:Result:=FResonance;
  end;
end;

procedure TFilter.GetParameterName(index: Int32; Text: PAnsiChar);
begin
  case index of
    0:VstStrncpy(Text,'Cutoff',7);
    1:VstStrncpy(Text,'Resonance',10);
  end;
end;

procedure TFilter.GetParameterLabel(index: Int32; _Label: PAnsiChar);
begin
  case index of
    0,1:VstStrncpy(_Label,'Hz',7);
  end;
end;

procedure TFilter.GetParameterDisplay(index: Int32; Text: PAnsiChar);
begin
  case index of
    0:Hz2String(FCutoff,Text,7);
    1:Hz2String(FResonance,Text,7);
  end;
end;

function TFilter.GetVendorString(Text: PAnsiChar): boolean;
begin
  VstStrncat(Text,'PeaZomboss',63);
  Result:=True;
end;

function TFilter.GetEffectName(Name: PAnsiChar): boolean;
begin
  VstStrncat(Name,'example5-filter',31);
  Result:=True;
end;

function TFilter.ProcessOneValue(inputvalue: double): double;
begin
  // There maybe bugs
  fbuf0:=fbuf0+FCutoff*(inputvalue-fbuf0+FFeedbackAmount * (fbuf0 - fbuf1));
  fbuf1:=fbuf1+FCutoff*(fbuf0-fbuf1);
  if FAttenuationMode=am12 then
  begin
    case FMode of
      fmLowPass: Result:=fbuf1;
      fmHighPass:Result:=inputvalue-fbuf0;
      fmBandPass:Result:=fbuf0-fbuf1;
    end;
  end else
  begin
    fbuf2:=fbuf2+FCutoff*(fbuf1-fbuf2);
    fbuf3:=fbuf3+FCutoff*(fbuf2-fbuf3);
    case FMode of
      fmLowPass: Result:=fbuf3;
      fmHighPass:Result:=inputvalue-fbuf3;
      fmBandPass:Result:=fbuf0-fbuf3;
    end;
  end;
end;

end.

