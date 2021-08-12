{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugbas
// Description : Base for vst2 plugin
// Created by  : PeaZomboss, 2021/07

// The base unit for plugin, usually not use directly
-------------------------------------------------------------------------------}

unit vst2plugbas;

{$I vcompiler.inc}

interface

uses
  vst2intf,Generics.Collections;

const
  iidIVBase:TGuid='{E6F6397F-1816-47D0-AF2C-E56DAD4DEAEC}';
  iidIVParam:TGuid='{4301340C-F67B-48E8-A67F-DD6BFBA6FC2E}';
  iidIVPreset:TGuid='{1EC361D2-C325-4A0C-8FC9-D17EB5EAF0B2}';

type
  TObjProc = procedure of object;
  TVendorSpecificObjFunc = function(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr of object;

  IVPlugBase = interface
    ['{E6F6397F-1816-47D0-AF2C-E56DAD4DEAEC}']
    function GetEffect:PAEffect;
    function CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr=0;const ptr:Pointer=nil;opt:single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetNames(const PlugName,Vendor,Product:string);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    function SampleRate:Single;
    function BlockSize:Integer;
    property Effect:PAEffect read GetEffect;
  end;

  TCustomParamDisplayObjFunc = function(index:integer):AnsiString of object;
  TVParameter = class;

  IVParam = interface
    ['{4301340C-F67B-48E8-A67F-DD6BFBA6FC2E}']
    function GetParameter(index:integer):Single;
    procedure SetParameter(index:integer;value:Single);
    procedure SetParamAutomated(index:integer;value:Single);
    procedure AddParameter(AValue:single;const AName,ALabel:AnsiString;CanBeAuto:Boolean=True;
      Properties:PVstParameterProperties=nil);
    procedure SetCustomParamDisplay(AProc:TCustomParamDisplayObjFunc);
    property Items[index:integer]:Single read GetParameter write SetParameter; default;
  end;

  PArrParams = ^TArrParams;
  TArrParams = array of single;

  IVPreset = interface
    ['{1EC361D2-C325-4A0C-8FC9-D17EB5EAF0B2}']
    procedure AddPreset(const AName:AnsiString;const ParamValues:TArrParams);

  end;

  { TVPlugBase }

  TVPlugBase = class(TInterfacedObject,IVPlugBase)
  private
    FHost:THostCallback;
    FSampleRate:Single;
    FBlockSize:Integer;
    FPlugCategory:TVstPlugCategory;
    FPlugName:AnsiString;
    FVendorName:AnsiString;
    FProductName:AnsiString;
    FVendorVersion:Int32;
    FVendorSpecific:TVendorSpecificObjFunc;
  protected
    FEffect:TAEffect;
    function GetEffect:PAEffect;
    function CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr=0;const ptr:Pointer=nil;opt:single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetNames(const PlugName,Vendor,Product:AnsiString);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    function SampleRate:Single;
    function BlockSize:Integer;
  public
    constructor Create(AHost:THostCallback;Obj:TObject);
    destructor Destroy;override;
    procedure SetSampleRate(AValue:Single);
    procedure SetBlockSize(AValue:Integer);
    function GetPlugCategory:Integer;
    function VendorSpecific(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr;
    property PlugName:AnsiString read FPlugName;
    property VendorName:AnsiString read FVendorName;
    property ProductName:AnsiString read FProductName;
    property VendorVersion:Int32 read FVendorVersion;
  end;

  { TVParameter }

  TVParameter = class
  private
    FValue:single;
    FName:AnsiString;
    FLabel:AnsiString;
    FCanBeAutomated:Boolean;
    FParamProperties:PVstParameterProperties;
  public
    constructor Create;
    property Value:single read FValue write FValue;
    property Name:AnsiString read FName write FName;
    property sLabel:AnsiString read FLabel write FLabel;
    property CanBeAutomated:Boolean read FCanBeAutomated write FCanBeAutomated;
    property ParamProperties:PVstParameterProperties read FParamProperties write FParamProperties;
  end;

  TVParamList = TObjectList<TVParameter>;

  { TVParamBase }

  TVParamBase = class(TVPlugBase,IVParam)
  private
    FParams:TVParamList;
    FCustomParamDisplay:TCustomParamDisplayObjFunc;
    FCurParamValues:TArrParams;
    function AddOne:TVParameter;
  protected
    FNumParam:Integer;
    function GetParameter(index:integer):Single;
    procedure SetParameter(index:integer;value:Single);
    procedure SetParamAutomated(index:integer;value:Single);
    procedure AddParameter(AValue:single;const AName,ALabel:AnsiString;CanBeAuto:Boolean=True;
      Properties:PVstParameterProperties=nil);
    procedure SetCustomParamDisplay(AProc:TCustomParamDisplayObjFunc);
  public
    constructor Create(AHost:THostCallback;Obj:TObject);
    destructor Destroy;override;
    function GetParamName(index:integer):AnsiString;
    function GetParamLabel(index:integer):AnsiString;
    function GetParamDisplay(index:integer):AnsiString;
    function CanBeAutomated(index:integer):Integer;
    function GetParameterProperties(index:integer;ptr:PVstParameterProperties):Integer;
    function GetChunk(const ptr:PArrParams):Integer;
    procedure SetChunk(const arr:TArrParams;ByteSize:Integer);
  end;

  { TVPreset }

  TVPreset = class
  private
    FName:AnsiString;
    FValues:TArrParams;
  public
    constructor Create(const AName:AnsiString;const ParamValues:TArrParams);
    property Name:AnsiString read FName write FName;
    property Values:TArrParams read FValues;
  end;

  TPresetList = TObjectList<TVPreset>;

  { TVPresetBase }

  TVPresetBase = class(TVParamBase,IVPreset)
  private
    FPresets:TPresetList;
    FCurPreset:Integer;
  protected
    FNumPreset:Integer;
    procedure AddPreset(const AName:AnsiString;const ParamValues:TArrParams);
  public
    constructor Create(AHost:THostCallback;Obj:TObject);
    destructor Destroy;override;
    function GetPreset:Integer;
    procedure SetPreset(NewPreset:Integer);
    function GetPresetName:AnsiString;
    procedure SetPresetName(const NewName:AnsiString);
    function GetPresetNameIndexed(index:Integer;ptr:PAnsiChar):Integer;
  end;

  TVPluginBase = class(TVPresetBase)

  end;


{$ifdef debug}
procedure dbgln(const log:string);overload;
procedure dbgln(const fmt:string;const args:array of const);overload;
{$endif}

implementation

uses
  sysutils,vst2plugin;

{$ifdef debug}
procedure dbgln(const log:string);
begin
  if IsConsole then Writeln('[PlugDbgLog]> ',log);
end;

procedure dbgln(const fmt:string;const args:array of const);
begin
  if IsConsole then WriteLn('[PlugDbgLog]> ',Format(fmt,args));
end;

{$endif}

{ TVPlugBase }

constructor TVPlugBase.Create(AHost:THostCallback;Obj:TObject);
begin
  //{$ifdef debug}dbgln('TVBase start create');{$endif}
  FHost:=AHost;
  // Init effect info
  with FEffect do
  begin
    Magic:=MakeLong(kVstMagic);
    Dispatcher:=VDispatcher;
    Process:=VProcess;
    SetParameter:=VSetParameter;
    GetParameter:=VGetParameter;
    NumPrograms:=0;
    NumParams:=0;
    NumInputs:=2;
    NumOutputs:=2;
    IORatio:=1;
    pObject:=Obj;
    UniqueID:=MakeLong('NoEf');
    Version:=1;
    ProcessReplacing:=VProcessRep;
{$ifdef VST_2_4_EXTENSIONS}
    Flags:=[effFlagsCanReplacing,effFlagsProgramChunks];
    ProcessDoubleReplacing:=VProcessRep64;
{$endif}
  end;
end;

function TVPlugBase.BlockSize:Integer;
begin
  Result:=FBlockSize;
end;

function TVPlugBase.CallHost(opcode:TAMOpcodes):IntPtr;
begin
  {$ifdef debug}dbgln('Call host with opcode: %s',[VstAMOpcode2Str(opcode)]);{$endif}
  Result:=FHost(@FEffect,Int32(opcode),0,0,nil,0);
end;

function TVPlugBase.CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr;const ptr:Pointer;opt:single):IntPtr;
begin
{$ifdef debug}
  dbgln('Call host with opcode: %s, index: %d, value: %d, ptr: %p, opt: %.5f',
    [VstAMOpcode2Str(opcode),index,value,ptr,opt]);
{$endif}
  Result:=FHost(@FEffect,Int32(opcode),index,value,ptr,opt);
end;

destructor TVPlugBase.Destroy;
begin
  //{$ifdef debug}dbgln('TVBase destroy');{$endif}
  inherited Destroy;
end;

function TVPlugBase.GetEffect:PAEffect;
begin
  Result:=@FEffect;
end;

function TVPlugBase.GetPlugCategory:Integer;
begin
  Result:=ord(FPlugCategory);
end;

function TVPlugBase.SampleRate:Single;
begin
  Result:=FSampleRate;
end;

procedure TVPlugBase.SetBlockSize(AValue:Integer);
begin
  FBlockSize:=AValue;
end;

procedure TVPlugBase.SetIONumber(InNumber,OutNumber:Int32);
begin
  FEffect.NumInputs:=InNumber;
  FEffect.NumOutputs:=OutNumber;
end;

procedure TVPlugBase.SetNames(const PlugName,Vendor,Product:AnsiString);
begin
  FPlugName:=PlugName;
  FVendorName:=Vendor;
  FProductName:=Product;
end;

procedure TVPlugBase.SetPlugCategory(APlugCateg:TVstPlugCategory);
begin
  FPlugCategory:=APlugCateg;
end;

procedure TVPlugBase.SetSampleRate(AValue:Single);
begin
  FSampleRate:=AValue;
end;

procedure TVPlugBase.SetUniqueID(const str4chars:AnsiString);
begin
  {$ifdef debug}dbgln('Set unique ID: %s',[str4chars]);{$endif}
  if length(str4chars)=4 then
    FEffect.UniqueID:=MakeLong(str4chars);
end;

procedure TVPlugBase.SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
begin
  FVendorSpecific:=AVendorSpecificFunc;
end;

procedure TVPlugBase.SetVersion(EffVer,VendorVer:Int32);
begin
  FEffect.Version:=EffVer;
  FVendorVersion:=VendorVer;
end;

function TVPlugBase.VendorSpecific(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr;
begin
  if Assigned(FVendorSpecific) then
    Result:=FVendorSpecific(Arg1,Arg2,Arg3,Arg4)
  else
    Result:=0;
end;


{ TVParameter }

constructor TVParameter.Create;
begin

end;


{ TVParamBase }

constructor TVParamBase.Create(AHost:THostCallback;Obj:TObject);
begin
  inherited Create(AHost,Obj);
  FNumParam:=0;
  FParams:=TVParamList.Create(True);
  //{$ifdef debug}dbgln('TVParamBase create');{$endif}
end;

procedure TVParamBase.AddParameter(AValue:single;const AName,ALabel:AnsiString;CanBeAuto:Boolean;
  Properties:PVstParameterProperties);
begin
  with AddOne do
  begin
    Value:=AValue;
    Name:=AName;
    sLabel:=ALabel;
    CanBeAutomated:=CanBeAuto;
    ParamProperties:=Properties;
  end;
end;

function TVParamBase.AddOne:TVParameter;
begin
  Result:=TVParameter.Create;
  FParams.Add(Result);
  Inc(FNumParam);
  FEffect.NumParams:=FNumParam;
end;

function TVParamBase.CanBeAutomated(index:integer):Integer;
begin
  if (index>=0) and (index<FNumParam) then
    Result:=Integer(FParams.Items[index].CanBeAutomated)
  else
    Result:=0;
end;

destructor TVParamBase.Destroy;
begin
  //{$ifdef debug}dbgln('TVParamBase destroy');{$endif}
  FParams.Free;
  inherited destroy;
end;

function TVParamBase.GetChunk(const ptr:PArrParams):Integer;
var
  i:integer;
begin
  SetLength(FCurParamValues,FNumParam);
  for i:=0 to FNumParam-1 do
    FCurParamValues[i]:=GetParameter(i);
  ptr^:=FCurParamValues;
  Result:=FNumParam*SizeOf(Single);
end;

function TVParamBase.GetParamDisplay(index:integer):AnsiString;
begin
  Result:='';
  if Assigned(FCustomParamDisplay) then
    Result:=FCustomParamDisplay(index);
  if Result='' then
    Result:=VstFloat2String(GetParameter(index));
end;

function TVParamBase.GetParameter(index:integer):Single;
begin
  if (index>=0) and (index<FNumParam) then
    Result:=FParams.Items[index].Value
  else
    Result:=0;
end;

function TVParamBase.GetParameterProperties(index:integer;ptr:PVstParameterProperties):Integer;
begin
  Result:=0;
  if (index>=0) and (index<FNumParam) and Assigned(FParams.Items[index].ParamProperties) then
  begin
    ptr^:=FParams.Items[index].ParamProperties^;
    Result:=1;
  end;
end;

function TVParamBase.GetParamLabel(index:integer):AnsiString;
begin
  if (index>=0) and (index<FNumParam) then
    Result:=FParams.Items[index].sLabel
  else
    Result:='';
end;

function TVParamBase.GetParamName(index:integer):AnsiString;
begin
  if (index>=0) and (index<FNumParam) then
    Result:=FParams.Items[index].Name
  else
    Result:='';
end;

procedure TVParamBase.SetChunk(const arr:TArrParams;ByteSize:Integer);
var
  num,i:Integer;
begin
  num:=ByteSize div SizeOf(Single);
  for i:=0 to num-1 do
    SetParameter(i,arr[i]);
end;

procedure TVParamBase.SetCustomParamDisplay(AProc:TCustomParamDisplayObjFunc);
begin
  FCustomParamDisplay:=AProc;
end;

procedure TVParamBase.SetParamAutomated(index:integer;value:Single);
begin
  SetParameter(index,value);
  CallHost(amAutomate,index,0,nil,value);
end;

procedure TVParamBase.SetParameter(index:integer;value:Single);
begin
  if (index>=0) and (index<FNumParam) then
    FParams.Items[index].Value:=value;
end;

{ TVPreset }

constructor TVPreset.Create(const AName:AnsiString;const ParamValues:TArrParams);
begin
  FName:=AName;
  FValues:=ParamValues;
end;

{ TVPresetBase }

constructor TVPresetBase.Create(AHost:THostCallback;Obj:TObject);
begin
  inherited Create(AHost,Obj);
  FCurPreset:=0;
  FNumPreset:=0;
  FPresets:=TPresetList.Create(True);
  //{$ifdef debug}dbgln('TVPresetBase create');{$endif}
end;

procedure TVPresetBase.AddPreset(const AName:AnsiString;const ParamValues:TArrParams);
begin
  if Length(ParamValues)=FNumParam then
  begin
    FPresets.Add(TVPreset.Create(AName,ParamValues));
    Inc(FNumPreset);
    FEffect.NumPrograms:=FNumPreset;
  end;
end;

destructor TVPresetBase.Destroy;
begin
  //{$ifdef debug}dbgln('TVPresetBase destroy');{$endif}
  SetLength(FCurParamValues,0);
  inherited Destroy;
end;

function TVPresetBase.GetPreset:Integer;
begin
  Result:=FCurPreset;
end;

function TVPresetBase.GetPresetName:AnsiString;
begin
  if FNumPreset>0 then
    Result:=FPresets.Items[FCurPreset].Name;
end;

function TVPresetBase.GetPresetNameIndexed(index:Integer;ptr:PAnsiChar):Integer;
begin
  if (index>=0) and (index<FNumPreset) then
  begin
    VstStrncpy(ptr,FPresets.Items[index].Name,23);
  end else Result:=0;
end;

procedure TVPresetBase.SetPreset(NewPreset:Integer);
var
  i:Integer;
begin
  if (NewPreset>=0) and (NewPreset<FNumPreset) then
  begin
    for i:=0 to FNumParam-1 do
      SetParameter(i,FPresets.Items[NewPreset].Values[i]);
    FCurPreset:=NewPreset;
  end;
end;

procedure TVPresetBase.SetPresetName(const NewName:AnsiString);
begin
  FPresets.Items[FCurPreset].Name:=NewName;
end;


end.

