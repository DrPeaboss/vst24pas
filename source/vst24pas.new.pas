{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst24pas.new
// Description : Extension for VSTPlugin class
// Created by  : PeaZomboss, 2021/7
-------------------------------------------------------------------------------}
unit vst24pas.new;

{$I vst24pas.inc}

interface

uses
  vst24pas.core,vst24pas.base,vst24pas.gui,forms;

type
  TOnParameterChange = procedure(index:int32;value:single) of object;

  PEffectInfo = ^TEffectInfo;
  TEffectInfo = record
    EffectName:string;
    Vendor:string;
    Product:string;
    VendorVersion:Int32;
  end;

  TDisplayMode = (dmFloat, dmdB, dmInteger, dmHz, dmMs, dmCustom, dmDonotDisplay);
  PParamInfo = ^TParamInfo;
  TParamInfo = record
    Value:single;
    Name:string;
    Lbl:string;
    DisplayMode:TDisplayMode;
  end;
  TOnCustomDisplay = function(index:int32):string of object;
  TArrParamInfo = array of TParamInfo;

  TSingleArray = array of single;

  PProgramInfo = ^TProgramInfo;
  TProgramInfo = record
    Name:string;
    Params:TSingleArray;
  end;
  TArrProgramInfo = array of TProgramInfo;

  { TPluginBase }

  TPluginBase = class(TVstPlugin)
  private
    FEffectInfo:TEffectInfo;
    FParamInfos:TArrParamInfo;
    FProgramInfos:TArrProgramInfo;
    FOnParameterChange:TOnParameterChange;
    FOnCustomDisplay:TOnCustomDisplay;
  protected
    procedure PlugInitParamInfo(index:int32;value:single;name:string='';lbl:string='';displaymode:TDisplayMode=dmFloat);
    procedure PlugInitEffectInfo(EffectName: string; Vendor: string; Product: string; VendorVersion: Int32=1);
    procedure PlugInitBasicInfo(UniqueID:Int32;Version:Int32=1);overload;
    procedure PlugInitBasicInfo(UniqueID:Int32;GUIClass:TVstGUIClass;Version:Int32=1);overload;
    procedure PlugInitBasicInfo(a,b,c,d:AnsiChar;Version:Int32=1);overload;
    procedure PlugInitBasicInfo(a,b,c,d:AnsiChar;GUIClass:TVstGUIClass;Version:Int32=1);overload;
    procedure PlugInitProgramInfo(index:int32;name:string;params:TSingleArray);
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    destructor Destroy; override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    function GetProductString(Text: PAnsiChar): boolean; override;
    function GetVendorVersion: Int32; override;
    procedure SetParameterAutomated(index: Int32; Value: single); override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterName(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterLabel(index: Int32; _Label: PAnsiChar); override;
    procedure SetProgram(_Program: Int32); override;
    procedure GetProgramName(Name: PAnsiChar); override;
    procedure SetProgramName(Name: PAnsiChar); override;
    function GetProgramNameIndexed(category, index: Int32; Text: PAnsiChar): boolean; override;
    function String2parameter(index: Int32; Text: PAnsiChar): boolean; override;
    // Version field in FCEffect
    procedure SetVersion(Version:Int32);
    property ParamInfos:TArrParamInfo read FParamInfos;
    property ProgramInfos:TArrProgramInfo read FProgramInfos;
    // Do not use it to change UI
    property OnParameterChange:TOnParameterChange read FOnParameterChange write FOnParameterChange;
    property OnCustomDisplay:TOnCustomDisplay read FOnCustomDisplay write FOnCustomDisplay;
  end;

implementation

uses
  vst24pas.utils,sysutils;

{ TPluginBase }

procedure TPluginBase.PlugInitParamInfo(index: int32; value: single; name: string; lbl: string;
  displaymode: TDisplayMode);
begin
  if index<FNumParams then
  begin
    FParamInfos[index].Value := value;
    FParamInfos[index].Name := name;
    FParamInfos[index].Lbl := lbl;
    FParamInfos[index].DisplayMode := displaymode;
  end;
end;

procedure TPluginBase.PlugInitEffectInfo(EffectName: string; Vendor: string; Product: string; VendorVersion: Int32);
begin
  FEffectInfo.EffectName := EffectName;
  FEffectInfo.Vendor := Vendor;
  FEffectInfo.Product := Product;
  FEffectInfo.VendorVersion := VendorVersion;
end;

procedure TPluginBase.PlugInitBasicInfo(UniqueID: Int32; Version: Int32);
begin
  VstPluginit(self,UniqueID);
  SetVersion(Version);
end;

procedure TPluginBase.PlugInitBasicInfo(UniqueID: Int32; GUIClass: TVstGUIClass; Version: Int32);
begin
  VstPluginit(self,UniqueID,GUIClass);
  SetVersion(Version);
end;

procedure TPluginBase.PlugInitBasicInfo(a, b, c, d: AnsiChar; Version: Int32);
begin
  PlugInitBasicInfo(MakeUniqueID(a,b,c,d),Version);
end;

procedure TPluginBase.PlugInitBasicInfo(a, b, c, d: AnsiChar; GUIClass: TVstGUIClass; Version: Int32);
begin
  PlugInitBasicInfo(MakeUniqueID(a,b,c,d),GUIClass,Version);
end;

procedure TPluginBase.PlugInitProgramInfo(index: int32; name: string; params: TSingleArray);
begin
  if (index<FNumPrograms) and (Length(params)=FNumParams) then
  begin
    FProgramInfos[index].Name := name;
    FProgramInfos[index].Params := params;
  end;
end;

constructor TPluginBase.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
var
  i:integer;
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetLength(FParamInfos,NumParams);
  SetLength(FProgramInfos,NumPrograms);
  for i:=0 to NumPrograms-1 do
    SetLength(FProgramInfos[i].Params,NumParams);
end;

destructor TPluginBase.Destroy;
var
  i:integer;
begin
  for i:=0 to FNumPrograms-1 do
    SetLength(FProgramInfos[i].Params,0);
  SetLength(FProgramInfos,0);
  SetLength(FParamInfos,0);
  inherited Destroy;
end;

function TPluginBase.GetEffectName(Name: PAnsiChar): boolean;
begin
  Result:=False;
  if FEffectInfo.EffectName<>'' then
  begin
    VstStrncpy(Name,PAnsiChar(FEffectInfo.EffectName),31);
    Result:=True;
  end;
end;

function TPluginBase.GetVendorString(Text: PAnsiChar): boolean;
begin
  Result:=False;
  if FEffectInfo.Vendor<>'' then
  begin
    VstStrncpy(Text,PAnsiChar(FEffectInfo.Vendor),63);
    Result:=True;
  end;
end;

function TPluginBase.GetProductString(Text: PAnsiChar): boolean;
begin
  Result:=False;
  if FEffectInfo.Product<>'' then
  begin
    VstStrncpy(Text,PAnsiChar(FEffectInfo.Product),63);
    Result:=True;
  end;
end;

function TPluginBase.GetVendorVersion: Int32;
begin
  Result := FEffectInfo.VendorVersion;
end;

procedure TPluginBase.SetVersion(Version: Int32);
begin
  FCEffect.Version := Version;
end;

procedure TPluginBase.SetParameterAutomated(index: Int32; Value: single);
begin
  inherited SetParameterAutomated(index, Value);
end;

procedure TPluginBase.SetParameter(index: Int32; Value: single);
begin
  if index<FNumParams then FParamInfos[index].Value := Value;
  if Assigned(FOnParameterChange) then FOnParameterChange(index, value);
end;

function TPluginBase.GetParameter(index: Int32): single;
begin
  if index<FNumParams then Result := FParamInfos[index].Value
  else Result := 0;
end;

procedure TPluginBase.GetParameterDisplay(index: Int32; Text: PAnsiChar);
begin
  if index<FNumParams then
    with FParamInfos[index] do
    case DisplayMode of
      dmdB:      dB2String(Value,Text,7);
      dmFloat:   Float2String(Value,Text,7);
      dmHz:      Hz2String(Value,Text,7);
      dmInteger: Int2String(Value,Text,7);
      dmMs:      Ms2String(Value,Text,7);
      dmCustom:
        if Assigned(FOnCustomDisplay) then VstStrncpy(Text,PAnsiChar(FOnCustomDisplay(index)),7);
      else ;
    end;
end;

procedure TPluginBase.GetParameterName(index: Int32; Text: PAnsiChar);
begin
  if index<FNumParams then VstStrncpy(Text,PAnsiChar(FParamInfos[index].Name),23);
end;

procedure TPluginBase.GetParameterLabel(index: Int32; _Label: PAnsiChar);
begin
  if index<FNumParams then VstStrncpy(_Label,PAnsiChar(FParamInfos[index].Lbl),7);
end;

procedure TPluginBase.SetProgram(_Program: Int32);
var
  i:integer;
begin
  inherited SetProgram(_Program);
  for i:=0 to FNumParams-1 do
    FParamInfos[i].Value := FProgramInfos[_Program].Params[i];
end;

procedure TPluginBase.GetProgramName(Name: PAnsiChar);
begin
  if FNumPrograms>0 then
    VstStrncpy(Name,PAnsiChar(FProgramInfos[FCurProgram].Name),23);
end;

procedure TPluginBase.SetProgramName(Name: PAnsiChar);
begin
  if FNumPrograms>0 then
    FProgramInfos[FCurProgram].Name := StrPas(Name);
end;

function TPluginBase.GetProgramNameIndexed(category, index: Int32; Text: PAnsiChar): boolean;
begin
  if index<FNumPrograms then
  begin
    VstStrncpy(Text,PAnsiChar(FProgramInfos[index].Name),23);
    Result := True;
  end;
end;

function TPluginBase.String2parameter(index: Int32; Text: PAnsiChar): boolean;
begin
  if index<FNumParams then
    Result:=TryStrToFloat(StrPas(Text),FParamInfos[index].Value);
end;

end.

