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
  end;

  TDisplayMode = (dmFloat, dmdB, dmInteger, dmHz, dmMs, dmCustom, dmDonotDisplay);
  PParamInfo = ^TParamInfo;
  TParamInfo = record
    Value:single;
    Name,
    Lbl:string;
    DisplayMode:TDisplayMode;
  end;
  TOnCustomDisplay = function(index:int32):string of object;

  TArrParamInfo = array of TParamInfo;

  { TPluginBase }

  TPluginBase = class(TVstPlugin)
  private
    FEffectInfo:TEffectInfo;
    FParamInfos:TArrParamInfo;
    FOnParameterChange:TOnParameterChange;
    FOnCustomDisplay:TOnCustomDisplay;
  protected
    procedure PlugInitParamInfo(index:int32;value:single;name:string='';lbl:string='';displaymode:TDisplayMode=dmFloat);
    procedure PlugInitEffectInfo(EffectName:string='';Vendor:string='';Product:string='');
    procedure PlugInitBasicInfo(UniqueID:Int32);overload;
    procedure PlugInitBasicInfo(UniqueID:Int32;GUIClass:TVstGUIClass);overload;
    procedure PlugInitBasicInfo(a,b,c,d:AnsiChar);overload;
    procedure PlugInitBasicInfo(a,b,c,d:AnsiChar;GUIClass:TVstGUIClass);overload;
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); override;
    destructor Destroy; override;
    function GetEffectName(Name: PAnsiChar): boolean; override;
    function GetVendorString(Text: PAnsiChar): boolean; override;
    function GetProductString(Text: PAnsiChar): boolean; override;
    procedure SetParameterAutomated(index: Int32; Value: single); override;
    procedure SetParameter(index: Int32; Value: single); override;
    function GetParameter(index: Int32): single; override;
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterName(index: Int32; Text: PAnsiChar); override;
    procedure GetParameterLabel(index: Int32; _Label: PAnsiChar); override;
    // Get index by name, return -1 if not found
    function IndexOf(Name:string):integer;
    property ParamInfos:TArrParamInfo read FParamInfos write FParamInfos;
    // Do not use it to change UI
    property OnParameterChange:TOnParameterChange read FOnParameterChange write FOnParameterChange;
    property OnCustomDisplay:TOnCustomDisplay read FOnCustomDisplay write FOnCustomDisplay;
  end;

implementation

uses
  vst24pas.utils;

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

procedure TPluginBase.PlugInitEffectInfo(EffectName: string; Vendor: string; Product: string);
begin
  FEffectInfo.EffectName := EffectName;
  FEffectInfo.Vendor := Vendor;
  FEffectInfo.Product := Product;
end;

procedure TPluginBase.PlugInitBasicInfo(UniqueID: Int32);
begin
  VstPluginit(self,UniqueID);
end;

procedure TPluginBase.PlugInitBasicInfo(UniqueID: Int32; GUIClass: TVstGUIClass);
begin
  VstPluginit(self,UniqueID,GUIClass);
end;

procedure TPluginBase.PlugInitBasicInfo(a, b, c, d: AnsiChar);
begin
  PlugInitBasicInfo(MakeUniqueID(a,b,c,d));
end;

procedure TPluginBase.PlugInitBasicInfo(a, b, c, d: AnsiChar; GUIClass: TVstGUIClass);
begin
  PlugInitBasicInfo(MakeUniqueID(a,b,c,d),GUIClass);
end;

constructor TPluginBase.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  inherited Create(VstHost, NumPrograms, NumParams);
  SetLength(FParamInfos,NumParams);
end;

destructor TPluginBase.Destroy;
begin
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
    if DisplayMode<>dmDonotDisplay then
    case DisplayMode of
      dmdB:      dB2String(Value,Text,7);
      dmFloat:   Float2String(Value,Text,7);
      dmHz:      Hz2String(Value,Text,7);
      dmInteger: Int2String(Value,Text,7);
      dmMs:      Ms2String(Value,Text,7);
      dmCustom:
        if Assigned(FOnCustomDisplay) then VstStrncpy(Text,PAnsiChar(FOnCustomDisplay(index)),7);
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

function TPluginBase.IndexOf(Name: string): integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to FNumParams-1 do
    if FParamInfos[i].Name = Name then
    begin
      Result:=i;
      break;
    end;
end;

end.

