{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugbas
// Description : Bases for vst2 plugin
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
  iidIVEditor:TGuid='{C34A39C2-8410-41BC-97E0-F473300CF1C5}';

type
  TObjProc = procedure of object;
  TVendorSpecificObjFunc = function(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr of object;

  IVPlugBase = interface
    ['{E6F6397F-1816-47D0-AF2C-E56DAD4DEAEC}']
    function CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr=0;const ptr:Pointer=nil;opt:single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetNames(const PlugName,Vendor,Product:string);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    procedure SetFlag(AFlag:TVstAEffectFlag;state:Boolean=True);
    procedure SetFlags(Flags:TVstAEffectFlags;state:Boolean=True);
    procedure SetCanDo(pcd:TPlugCanDo;state:Boolean=True);
    function SampleRate:Single;
    function BlockSize:Integer;
  end;

  TCustomParamDisplayObjFunc = function(index:integer):AnsiString of object;

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

  IVEditor = interface
    ['{C34A39C2-8410-41BC-97E0-F473300CF1C5}']
    function IsOpen:Boolean;
    procedure EditBegin(index:integer);
    procedure EditEnd(index:integer);
    procedure SetIdle(IdleProc:TObjProc);
    procedure SetGui(FrmClass:TClass);
    function GetGui:TObject;
    property Gui:TObject read GetGui;
  end;

  { TVPlugBase }

  TVPlugBase = class(TInterfacedObject,IVPlugBase)
  private
    FHost:THostCallback;
    FSampleRate:Single;
    FBlockSize:Integer;
    FPrecision:TVstProcessPrecision;
    FPlugCategory:TVstPlugCategory;
    FPlugName:AnsiString;
    FVendorName:AnsiString;
    FProductName:AnsiString;
    FVendorVersion:Int32;
    FVendorSpecific:TVendorSpecificObjFunc;
    FCanDos:TPlugCanDos;
  protected
    FEffect:TAEffect;
    function CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr=0;const ptr:Pointer=nil;opt:single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetNames(const PlugName,Vendor,Product:AnsiString);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    procedure SetFlag(AFlag:TVstAEffectFlag;state:Boolean=True);
    procedure SetFlags(Flags:TVstAEffectFlags;state:Boolean=True);
    procedure SetCanDo(pcd:TPlugCanDo;state:Boolean=True);
    function SampleRate:Single;
    function BlockSize:Integer;
  public
    constructor Create(AHost:THostCallback;Plugin:TObject);
    destructor Destroy;override;
    function GetEffect:PAEffect;
    procedure SetSampleRate(AValue:Single);
    procedure SetBlockSize(AValue:Integer);
    procedure SetProcessPrecision(AValue:Integer);
    function GetPlugCategory:Integer;
    function VendorSpecific(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr;
    function CanDo(szcd:PAnsiChar):Integer;
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
    constructor Create(AHost:THostCallback;Plugin:TObject);
    destructor Destroy;override;
    function GetParamName(index:integer):AnsiString;
    function GetParamLabel(index:integer):AnsiString;
    function GetParamDisplay(index:integer):AnsiString;
    function CanBeAutomated(index:integer):Integer;
    function GetParameterProperties(index:integer;ptr:PVstParameterProperties):Integer;
    function GetChunk(ptr:PArrParams):Integer;
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
    constructor Create(AHost:THostCallback;Plugin:TObject);
    destructor Destroy;override;
    function GetPreset:Integer;
    procedure SetPreset(NewPreset:Integer);
    function GetPresetName:AnsiString;
    procedure SetPresetName(ptr:PAnsiChar);
    function GetPresetNameIndexed(index:Integer;ptr:PAnsiChar):Integer;
  end;

  TVPluginBase = class(TVPresetBase)

  end;

  TVEditorBase = class(TInterfacedObject)
  public
    procedure Open(ParentHandle:Pointer);dynamic;abstract;
    procedure Close;dynamic;abstract;
    procedure GetRect(const Rect:PPERect);virtual;abstract;
    procedure Idle;virtual;abstract;
  end;


{$ifdef debug}
procedure dbgln(const log:string);overload;
procedure dbgln(const fmt:string;const args:array of const);overload;
{$endif}

implementation

uses
  sysutils
  {$ifdef debug},classes{$endif};

{$ifdef debug}
procedure dbgln(const log:string);
//var
//  fn:String;
begin
  if IsConsole then Writeln('[PlugDbgLog]> ',log);
  {
  fn:=GetUserDir+'vst24paslog.log';
  if not FileExists(fn) then
    FileClose(FileCreate(fn));
  with TStringList.Create do
  begin
    LoadFromFile(fn);
    Add('[PlugDbgLog]> '+log);
    SaveToFile(fn);
    Free;
  end; }
end;

procedure dbgln(const fmt:string;const args:array of const);
//var
//  fn:String;
begin
  if IsConsole then WriteLn('[PlugDbgLog]> ',Format(fmt,args));
  {
  fn:=GetUserDir+'vst24paslog.log';
  if not FileExists(fn) then
    FileClose(FileCreate(fn));
  with TStringList.Create do
  begin
    LoadFromFile(fn);
    Add('[PlugDbgLog]> '+Format(fmt,args));
    SaveToFile(fn);
    Free;
  end; }
end;

{$endif}

{ TVPlugBase }

constructor TVPlugBase.Create(AHost:THostCallback;Plugin:TObject);
begin
  //{$ifdef debug}dbgln('TVBase start create');{$endif}
  FHost:=AHost;
  // Init effect info
  with FEffect do
  begin
    Magic:=MakeLong(kVstMagic);
    NumInputs:=2;
    NumOutputs:=2;
    IORatio:=1;
    Obj:=Plugin;
    UniqueID:=MakeLong('NoEf');
    Version:=1;
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

procedure TVPlugBase.SetProcessPrecision(AValue:Integer);
begin
  if (AValue=0) or (AValue=1) then
    FPrecision:=TVstProcessPrecision(AValue);
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

procedure TVPlugBase.SetFlag(AFlag:TVstAEffectFlag;state:Boolean);
begin
  if state then
    Include(FEffect.Flags,AFlag)
  else
    Exclude(FEffect.Flags,AFlag);
end;

procedure TVPlugBase.SetFlags(Flags:TVstAEffectFlags;state:Boolean);
begin
  if state then
    FEffect.Flags:=FEffect.Flags+Flags
  else
    FEffect.Flags:=FEffect.Flags-Flags;
end;

procedure TVPlugBase.SetCanDo(pcd:TPlugCanDo;state:Boolean);
begin
  if state then
    Include(FCanDos,pcd)
  else
    Exclude(FCanDos,pcd);
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

function TVPlugBase.CanDo(szcd:PAnsiChar):Integer;
begin
  if VstString2PlugCanDo(szcd) in FCanDos then
    Result:=1
  else
    Result:=-1;;
end;


{ TVParameter }

constructor TVParameter.Create;
begin

end;


{ TVParamBase }

constructor TVParamBase.Create(AHost:THostCallback;Plugin:TObject);
begin
  inherited Create(AHost,Plugin);
  FNumParam:=0;
  FParams:=TVParamList.Create(True);
  SetFlag(effFlagsProgramChunks);
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
  FParams.Free;
  inherited destroy;
end;

function TVParamBase.GetChunk(ptr:PArrParams):Integer;
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
  if (index>=0) and (index<FNumParam) and Assigned(FParams.Items[index].ParamProperties) then
  begin
    ptr^:=FParams.Items[index].ParamProperties^;
    Result:=1;
  end else Result:=0;
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

constructor TVPresetBase.Create(AHost:THostCallback;Plugin:TObject);
begin
  inherited Create(AHost,Plugin);
  FCurPreset:=0;
  FNumPreset:=0;
  FPresets:=TPresetList.Create(True);
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
  FPresets.Free;
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

procedure TVPresetBase.SetPresetName(ptr:PAnsiChar);
begin
  FPresets.Items[FCurPreset].Name:=StrPas(ptr);
end;


end.

