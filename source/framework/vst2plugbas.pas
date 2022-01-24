{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugbas
// Description : Bases for vst2 plugin
// Created by  : PeaZomboss, 2021/07

// The base unit for plugin, usually not use directly
-------------------------------------------------------------------------------}

unit vst2plugbas;

{$I vst2def.inc}

interface

uses
  vst2intf,Generics.Collections;

type
  TObjProc = procedure of object;
  TVendorSpecificObjFunc = function(Arg1:Int32;Arg2:IntPtr;Arg3:Pointer;Arg4:Single):IntPtr of object;

  IVHost = interface
    ['{E2865726-6113-4C71-B5FF-8D9F2F944EDD}']
    function GetHostVersion:Int32;
    function GetCurrentUniqueID:Int32;
    procedure HostIdle;
    function GetTimeInfo(flags:TVstTimeInfoFlags):PVstTimeInfo;
    function IOChanged:Boolean;
    procedure HostProcessEvents(const events:TVstEvents);
    procedure SizeWindow(Width,Height:Integer);
    function UpdateSampleRate:Single;
    function UpdateBlockSize:Int32;
    function GetCurrentProcessLevel:Int32;
    function GetAutomationState:Int32;
    function OfflineStart(const AudioFiles:TVstAudioFile;AudioFileNum,NewAudioFilesNum:Int32):Boolean;
    function OfflineRead(const task:TVstOfflineTask;option:TVstOfflineOption;ReadSource:Boolean):Boolean;
    function OfflineWrite(const task:TVstOfflineTask;option:TVstOfflineOption):Boolean;
    function OfflineGetCurrentPass:Int32;
    function OfflineGetCurrentMetaPass:Int32;
    function GetHostVendorString:AnsiString;
    function GetHostProductString:AnsiString;
    function GetHostVendorVersion:Int32;
    function HostVendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
    function HostCanDo(text:AnsiString):Boolean;
    function GetHostLanguage:Int32;
    function GetDirectory:AnsiString;
    function UpdateDisplay:Boolean;
    function BeginEdit(index:Int32):Boolean;
    function EndEdit(index:Int32):Boolean;
    function HostOpenFileSelector(const FileSelect:TVstFileSelect):Boolean;
    function HostCloseFileSelector(const FileSelect:TVstFileSelect):Boolean;
  end;

  IVPlugBase = interface
    ['{E6F6397F-1816-47D0-AF2C-E56DAD4DEAEC}']
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetNames(const PlugName,Vendor,Product:AnsiString);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    procedure SetFlag(AFlag:TVstAEffectFlag;state:Boolean=True);
    procedure SetFlags(Flags:TVstAEffectFlags;state:Boolean=True);
    procedure SetCanDo(pcd:TPlugCanDo;state:Boolean=True);
    procedure SetAsSynth;
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
    procedure BindParameter(index:Integer;var BindVar:Single);
    procedure BindLastParameter(var BindVar:Single);
    procedure SetCustomParamDisplay(AProc:TCustomParamDisplayObjFunc);
    property Items[index:integer]:Single read GetParameter write SetParameter; default;
  end;

  PArrParams = ^TArrParams;
  TArrParams = array of single;
  TArrAnsiString = array of AnsiString;

  IVPreset = interface
    ['{1EC361D2-C325-4A0C-8FC9-D17EB5EAF0B2}']
    procedure DiasableDAWPreset;
    // Return true when preset number or name changed
    function NumberNameChanged:Boolean;
    function GetCurPreset:Integer;
    function GetPresetNum:Integer;
    function GetPresetName:AnsiString;
    function GetPresetNameArray:TArrAnsiString;
    // Set default preset that used by InitPreset and InsertPreset
    procedure SetDefaultPreset(const ParamValues:TArrParams);
    procedure SetPreset(index:Integer);overload;
    // Set preset by name (if there are duplicate names, only use the first one)
    procedure SetPreset(const AName:AnsiString);overload;
    procedure AddPreset(const AName:AnsiString;const ParamValues:TArrParams);
    procedure RenamePreset(const AName:AnsiString);
    procedure InitPreset;
    procedure InsertPreset;
    procedure DeletePreset;
    procedure RandomPreset;
    // Change to next preset and return the index
    function NextPreset:Integer;
    // Change to previous preset and return the index
    function PrevPreset:Integer;
    procedure CopyPreset;
    procedure PastePreset;
    //procedure LoadFromFile;
    //procedure SaveToFile;
  end;

  TMidiStatus = packed record
    IsNew:Boolean;
    Command:Byte;
    Note:Byte;
    Velocity:Byte;
  end;

  TOnMidiInProc = procedure(Command,Note,Velocity:Byte) of object;

  IVMidi = interface
    ['{18FA3F13-B878-4B3D-B390-EAE022FAC248}']
    function QueryNewMidiData(out Command,Note,Velocity:Byte):Boolean;
    procedure SetOnMidiIn(MidiInProc:TOnMidiInProc);
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

  iidIVHost     = IVHost;
  iidIVPlugBase = IVPlugBase;
  iidIVParam    = IVParam;
  iidIVPreset   = IVPreset;
  iidIVMidi     = IVMidi;
  //iidIVEditor = IVEditor;

  { TVPlugBase }

  TVPlugBase = class(TInterfacedObject,IVPlugBase,IVHost)
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
    FDefaultIO:Boolean;
    function CallHost(opcode:TAMOpcodes;index:Int32;const value:IntPtr=0;const ptr:Pointer=nil;opt:single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
  protected
    FEffect:TAEffect;
    procedure SetUniqueID(const str4chars:AnsiString);
    procedure SetNames(const PlugName,Vendor,Product:AnsiString);
    procedure SetVersion(EffVer,VendorVer:Int32);
    procedure SetPlugCategory(APlugCateg:TVstPlugCategory);
    procedure SetIONumber(InNumber,OutNumber:Int32);
    procedure SetVendorSpecific(AVendorSpecificFunc:TVendorSpecificObjFunc);
    procedure SetFlag(AFlag:TVstAEffectFlag;state:Boolean=True);
    procedure SetFlags(Flags:TVstAEffectFlags;state:Boolean=True);
    procedure SetCanDo(pcd:TPlugCanDo;state:Boolean=True);
    procedure SetAsSynth;
    function SampleRate:Single;
    function BlockSize:Integer;
  protected
    function GetHostVersion:Int32;
    function GetCurrentUniqueID:Int32;
    procedure HostIdle;
    function GetTimeInfo(flags:TVstTimeInfoFlags):PVstTimeInfo;
    function IOChanged:Boolean;
    procedure HostProcessEvents(const events:TVstEvents);
    procedure SizeWindow(Width,Height:Integer);
    function UpdateSampleRate:Single;
    function UpdateBlockSize:Int32;
    function GetCurrentProcessLevel:Int32;
    function GetAutomationState:Int32;
    function OfflineStart(const AudioFiles:TVstAudioFile;AudioFileNum,NewAudioFilesNum:Int32):Boolean;
    function OfflineRead(const task:TVstOfflineTask;option:TVstOfflineOption;ReadSource:Boolean):Boolean;
    function OfflineWrite(const task:TVstOfflineTask;option:TVstOfflineOption):Boolean;
    function OfflineGetCurrentPass:Int32;
    function OfflineGetCurrentMetaPass:Int32;
    function GetHostVendorString:AnsiString;
    function GetHostProductString:AnsiString;
    function GetHostVendorVersion:Int32;
    function HostVendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
    function HostCanDo(text:AnsiString):Boolean;
    function GetHostLanguage:Int32;
    function GetDirectory:AnsiString;
    function UpdateDisplay:Boolean;
    function BeginEdit(index:Int32):Boolean;
    function EndEdit(index:Int32):Boolean;
    function HostOpenFileSelector(const FileSelect:TVstFileSelect):Boolean;
    function HostCloseFileSelector(const FileSelect:TVstFileSelect):Boolean;
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
    property DefaultIO:Boolean read FDefaultIO;
  end;

  { TVParameter }

  TVParameter = class
  private type
    TValue = record
    case Byte of
      0:(Addr:PSingle);
      1:(Value:Single);
    end;
  private
    FIsBind:Boolean;
    FValue:TValue;
    FName:AnsiString;
    FLabel:AnsiString;
    FCanBeAutomated:Boolean;
    FParamProperties:PVstParameterProperties;
    function GetValue:Single;
    procedure SetValue(AValue:Single);
  public
    constructor Create;
    procedure BindValue(var BindVar:Single);
    property Value:single read GetValue write SetValue;
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
    procedure BindParameter(index:Integer;var BindVar:Single);
    procedure BindLastParameter(var BindVar:Single);
    procedure SetCustomParamDisplay(AProc:TCustomParamDisplayObjFunc);
  public
    constructor Create(AHost:THostCallback;Plugin:TObject);
    destructor Destroy;override;
    function GetParamName(index:integer):AnsiString;
    function GetParamLabel(index:integer):AnsiString;
    function GetParamDisplay(index:integer):AnsiString;
    function CanBeAutomated(index:integer):Integer;
    function GetParameterProperties(index:integer;const ptr:PVstParameterProperties):Integer;
    function GetChunk(const ptr:PArrParams):Integer;
    procedure SetChunk(arr:Pointer;ByteSize:Integer);
  end;

  { TVPreset }

  TVPreset = class
  private
    FName:AnsiString;
    FValues:TArrParams;
    FLength:Integer;
  public
    constructor Create(const AName:AnsiString;const ParamValues:TArrParams;Len:Integer);
    procedure CopyParams(const Params:TArrParams);
    property Name:AnsiString read FName write FName;
    property Values:TArrParams read FValues;
  end;

  TPresetList = TObjectList<TVPreset>;

  { TVPresetBase }

  TVPresetBase = class(TVParamBase,IVPreset)
  private
    FPresets:TPresetList;
    FCurPreset:Integer;
    FDefaultPreset:TArrParams;
    FClipboard:TArrParams;
    FDisableDAWPreset:Boolean;
    FNumberNameChanged:Boolean;
    procedure SetNumberNameChanged;
    procedure InternalLoadPreset(const Params:TArrParams);
    function InternalStorePreset:TArrParams;inline;
    procedure InternalSavePreset;
    procedure GenerateDefaultPreset;
    procedure LoadDefaultPreset;
  protected
    FNumPreset:Integer;
    procedure DiasableDAWPreset;
    function NumberNameChanged:Boolean;
    function GetPresetNum:Integer;
    function GetPresetNameArray:TArrAnsiString;
    procedure SetDefaultPreset(const ParamValues:TArrParams);
    procedure SetPreset(const AName:AnsiString);overload;
    procedure AddPreset(const AName:AnsiString;const ParamValues:TArrParams);
    procedure RenamePreset(const AName:AnsiString);
    procedure InitPreset;
    procedure InsertPreset;
    procedure DeletePreset;
    procedure RandomPreset;
    function NextPreset:Integer;
    function PrevPreset:Integer;
    procedure CopyPreset;
    procedure PastePreset;
    //procedure LoadFromFile;
    //procedure SaveToFile;
  public
    constructor Create(AHost:THostCallback;Plugin:TObject);
    destructor Destroy;override;
    function GetCurPreset:Integer;
    procedure SetPreset(index:Integer);overload;
    function GetPresetName:AnsiString;
    procedure SetPresetName(ptr:PAnsiChar);
    function GetPresetNameIndexed(index:Integer;const ptr:PAnsiChar):Integer;
  end;

  { TVMidiBase }

  TVMidiBase = class(TVPresetBase,IVMidi)
  private
    FMidiStatus:TMidiStatus;
    FOnMidiIn:TOnMidiInProc;
    procedure ProcessMidiEvent(const Event:TVstMidiEvent);
  protected
    function QueryNewMidiData(out Command,Note,Velocity:Byte):Boolean;
    procedure SetOnMidiIn(MidiInProc:TOnMidiInProc);
  public
    procedure ProcessEvents(Events:PVstEvents);
  end;

  TVPluginBase = class(TVMidiBase)

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
  sysutils,vst2utils
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
  FSampleRate:=44100;
  FBlockSize:=1024;
  FDefaultIO:=True;
  //FCanDos:=[pcdTemp];
end;

function TVPlugBase.BlockSize:Integer;
begin
  Result:=FBlockSize;
end;

function TVPlugBase.GetHostVersion:Int32;
begin
  Result:=CallHost(amVersion);
end;

function TVPlugBase.GetCurrentUniqueID:Int32;
begin
  Result:=CallHost(amCurrentId);
end;

procedure TVPlugBase.HostIdle;
begin
  CallHost(amIdle);
end;

function TVPlugBase.GetTimeInfo(flags:TVstTimeInfoFlags):PVstTimeInfo;
begin
  Result:=FromIntPtr(CallHost(amGetTime,0,IntPtr(flags)));
end;

function TVPlugBase.IOChanged:Boolean;
begin
  Result:=CallHost(amIOChanged)<>0;
end;

procedure TVPlugBase.HostProcessEvents(const events:TVstEvents);
begin
  CallHost(amProcessEvents,0,0,@events);
end;

procedure TVPlugBase.SizeWindow(Width,Height:Integer);
begin
  CallHost(amSizeWindow,Width,Height);
end;

function TVPlugBase.UpdateSampleRate:Single;
var
  res:IntPtr;
begin
  res:=CallHost(amGetSampleRate);
  if res>0 then FSampleRate:=res;
  Result:=FSampleRate;
end;

function TVPlugBase.UpdateBlockSize:Int32;
var
  res:IntPtr;
begin
  res:=CallHost(amGetBlockSize);
  if res>0 then FBlockSize:=res;
  Result:=FBlockSize;
end;

function TVPlugBase.GetCurrentProcessLevel:Int32;
begin
  Result:=CallHost(amGetCurrentProcessLevel);
end;

function TVPlugBase.GetAutomationState:Int32;
begin
  Result:=CallHost(amGetAutomationState);
end;

function TVPlugBase.OfflineStart(const AudioFiles:TVstAudioFile;AudioFileNum,NewAudioFilesNum:Int32):Boolean;
begin
  Result:=CallHost(amOfflineStart,NewAudioFilesNum,AudioFileNum,@AudioFiles)<>0;
end;

function TVPlugBase.OfflineRead(const task:TVstOfflineTask;option:TVstOfflineOption;ReadSource:Boolean
  ):Boolean;
begin
  Result:=CallHost(amOfflineRead,Int32(ReadSource),IntPtr(option),@task)<>0;
end;

function TVPlugBase.OfflineWrite(const task:TVstOfflineTask;option:TVstOfflineOption):Boolean;
begin
  Result:=CallHost(amOfflineWrite,0,IntPtr(option),@task)<>0;
end;

function TVPlugBase.OfflineGetCurrentPass:Int32;
begin
  Result:=CallHost(amOfflineGetCurrentPass);
end;

function TVPlugBase.OfflineGetCurrentMetaPass:Int32;
begin
  Result:=CallHost(amOfflineGetCurrentMetaPass);
end;

function TVPlugBase.GetHostVendorString:AnsiString;
var
  buffer:array[1..kVstMaxVendorStrLen] of AnsiChar;
begin
  CallHost(amGetVendorString,0,0,@buffer);
  buffer[kVstMaxVendorStrLen]:=#0;
  Result:=buffer;
end;

function TVPlugBase.GetHostProductString:AnsiString;
var
  buffer:array[1..kVstMaxProductStrLen] of AnsiChar;
begin
  CallHost(amGetVendorString,0,0,@buffer);
  buffer[kVstMaxProductStrLen]:=#0;
  Result:=buffer;
end;

function TVPlugBase.GetHostVendorVersion:Int32;
begin
  Result:=CallHost(amGetVendorVersion);
end;

function TVPlugBase.HostVendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=CallHost(amVendorSpecific,index,value,ptr,opt);
end;

function TVPlugBase.HostCanDo(text:AnsiString):Boolean;
begin
  Result:=CallHost(amCanDo,0,0,PAnsiChar(text))=1;
end;

function TVPlugBase.GetHostLanguage:Int32;
begin
  Result:=CallHost(amGetLanguage);
end;

function TVPlugBase.GetDirectory:AnsiString;
begin
  Result:=PAnsiChar(FromIntPtr(CallHost(amGetDirectory)));
end;

function TVPlugBase.UpdateDisplay:Boolean;
begin
  Result:=CallHost(amUpdateDisplay)<>0;
end;

function TVPlugBase.BeginEdit(index:Int32):Boolean;
begin
  Result:=CallHost(amBeginEdit,index)<>0;
end;

function TVPlugBase.EndEdit(index:Int32):Boolean;
begin
  Result:=CallHost(amEndEdit,index)<>0;
end;

function TVPlugBase.HostOpenFileSelector(const FileSelect:TVstFileSelect):Boolean;
begin
  Result:=CallHost(amOpenFileSelector,0,0,@FileSelect)<>0;
end;

function TVPlugBase.HostCloseFileSelector(const FileSelect:TVstFileSelect):Boolean;
begin
  Result:=CallHost(amCloseFileSelector,0,0,@FileSelect)<>0;
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
  FDefaultIO:=(InNumber=2) and (OutNumber=2);
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

procedure TVPlugBase.SetAsSynth;
begin
  SetFlag(effFlagsIsSynth);
  SetPlugCategory(kPlugCategSynth);
  SetCanDo(pcdReceiveVstMidiEvent);
  SetIONumber(0,2);
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
    Result:=-1;
end;


{ TVParameter }

function TVParameter.GetValue:Single;
begin
  if FIsBind then
    Result:=FValue.Addr^
  else
    Result:=FValue.Value;
end;

procedure TVParameter.SetValue(AValue:Single);
begin
  if FIsBind then
    FValue.Addr^:=AValue
  else
    FValue.Value:=AValue;
end;

constructor TVParameter.Create;
begin

end;

procedure TVParameter.BindValue(var BindVar:Single);
begin
  FIsBind:=True;
  BindVar:=FValue.Value;
  FValue.Addr:=@BindVar;
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

procedure TVParamBase.BindParameter(index:Integer;var BindVar:Single);
begin
  if (index>=0) and (index<FNumParam) then
    FParams.Items[index].BindValue(BindVar);
end;

procedure TVParamBase.BindLastParameter(var BindVar:Single);
begin
  FParams.Last.BindValue(BindVar);
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

function TVParamBase.GetParameterProperties(index:integer;const ptr:PVstParameterProperties):Integer;
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

procedure TVParamBase.SetChunk(arr:Pointer;ByteSize:Integer);
var
  num,i:Integer;
begin
  if Assigned(arr) then
  begin
    num:=ByteSize div SizeOf(Single);
    for i:=0 to num-1 do
      SetParameter(i,TArrParams(arr)[i]);
  end;
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

constructor TVPreset.Create(const AName:AnsiString;const ParamValues:TArrParams;Len:Integer);
begin
  FName:=AName;
  FLength:=Len;
  SetLength(FValues,Len);
  CopyParams(ParamValues);
end;

procedure TVPreset.CopyParams(const Params:TArrParams);
var
  i:Integer;
begin
  if Length(Params)=FLength then
    for i:=0 to FLength-1 do
      FValues[i]:=Params[i];
end;

{ TVPresetBase }

constructor TVPresetBase.Create(AHost:THostCallback;Plugin:TObject);
begin
  inherited Create(AHost,Plugin);
  FCurPreset:=0;
  FNumPreset:=0;
  FPresets:=TPresetList.Create(True);
end;

procedure TVPresetBase.SetNumberNameChanged;
begin
  FNumberNameChanged:=True;
end;

procedure TVPresetBase.InternalLoadPreset(const Params:TArrParams);
var
  i:Integer;
begin
  for i:=0 to FNumParam-1 do
    SetParameter(i,Params[i]);
end;

function TVPresetBase.InternalStorePreset:TArrParams;
var
  i:Integer;
begin
  Result:=nil;
  Setlength(Result,FNumParam);
  for i:=0 to FNumParam-1 do
    Result[i]:=GetParameter(i);
end;

procedure TVPresetBase.LoadDefaultPreset;
begin
  GenerateDefaultPreset;
  InternalLoadPreset(FDefaultPreset);
end;

procedure TVPresetBase.DiasableDAWPreset;
begin
  FDisableDAWPreset:=True;
end;

function TVPresetBase.NumberNameChanged:Boolean;
begin
  Result:=FNumberNameChanged;
  if Result then
    FNumberNameChanged:=False;
end;

function TVPresetBase.GetCurPreset:Integer;
begin
  Result:=FCurPreset;
end;

function TVPresetBase.GetPresetNum:Integer;
begin
  Result:=FNumPreset;
end;

function TVPresetBase.GetPresetNameArray:TArrAnsiString;
var
  i:Integer;
begin
  Result:=nil;
  if FNumPreset>0 then
  begin
    SetLength(Result,FNumPreset);
    for i:=0 to FNumPreset-1 do
      Result[i]:=FPresets.Items[i].Name;
  end;
end;

procedure TVPresetBase.AddPreset(const AName:AnsiString;const ParamValues:TArrParams);
begin
  if Length(ParamValues)=FNumParam then
  begin
    Inc(FNumPreset);
    FPresets.Add(TVPreset.Create(AName,ParamValues,FNumParam));
    if not FDisableDAWPreset then
      FEffect.NumPrograms:=FNumPreset;
    SetNumberNameChanged;
  end;
end;

procedure TVPresetBase.InsertPreset;
begin
  GenerateDefaultPreset;
  if FNumPreset=0 then
  begin
    AddPreset('',FDefaultPreset);
    InitPreset;
    SetNumberNameChanged;
  end
  else if FCurPreset<FNumPreset then
  begin
    FPresets.Insert(FCurPreset,TVPreset.Create('',FDefaultPreset,FNumParam));
    InternalSavePreset;
    InitPreset;
    Inc(FNumPreset);
    SetNumberNameChanged;
  end;
end;

procedure TVPresetBase.SetDefaultPreset(const ParamValues:TArrParams);
begin
  if Length(ParamValues)=FNumParam then
  begin
    FDefaultPreset:=ParamValues;
  end;
end;

procedure TVPresetBase.SetPreset(const AName:AnsiString);
var
  i:Integer;
begin
  for i:=0 to FPresets.Count-1 do
    if FPresets.Items[i].Name=AName then
    begin
      SetPreset(i);
      Break;
    end;
end;

procedure TVPresetBase.RandomPreset;
var
  params:TArrParams;
  i:Integer;
begin
  params:=nil;
  SetLength(params,FNumParam);
  Randomize;
  for i:=0 to FNumParam-1 do
    params[i]:=Random;
  InternalLoadPreset(params);
end;

procedure TVPresetBase.InitPreset;
begin
  if FCurPreset<FNumPreset then
  begin
    LoadDefaultPreset;
    FPresets.Items[FCurPreset].CopyParams(FDefaultPreset);
    FPresets.Items[FCurPreset].Name:='Init';
    SetNumberNameChanged;
  end;
end;

procedure TVPresetBase.RenamePreset(const AName:AnsiString);
begin
  if (AName<>'') and (FCurPreset<FNumPreset) then
  begin
    FPresets.Items[FCurPreset].Name:=AName;
    SetNumberNameChanged;
  end;
end;

procedure TVPresetBase.DeletePreset;
begin
  if FNumPreset>0 then
  begin
    FPresets.Delete(FCurPreset);
    Dec(FNumPreset);
    if (FCurPreset=FNumPreset) then
      Dec(FCurPreset);
    if FCurPreset<0 then
      FCurPreset:=0;
    if FPresets.Count>0 then
      InternalLoadPreset(FPresets.Items[FCurPreset].Values)
    else
      LoadDefaultPreset;
    SetNumberNameChanged;
  end;
end;

procedure TVPresetBase.InternalSavePreset;
begin
  FPresets.Items[FCurPreset].CopyParams(InternalStorePreset);
end;

procedure TVPresetBase.GenerateDefaultPreset;
begin
  if not Assigned(FDefaultPreset) then
  SetLength(FDefaultPreset,FNumParam);
end;

function TVPresetBase.NextPreset:Integer;
begin
  if FCurPreset<FNumPreset then
  begin
    InternalSavePreset;
    if FCurPreset=FNumPreset-1 then
      FCurPreset:=0
    else
      Inc(FCurPreset);;
    InternalLoadPreset(FPresets.Items[FCurPreset].Values);
    Result:=FCurPreset;
  end;
end;

function TVPresetBase.PrevPreset:Integer;
begin
  if FCurPreset<FNumPreset then
  begin
    InternalSavePreset;
    if FCurPreset=0 then
      FCurPreset:=FNumPreset-1
    else
      Dec(FCurPreset);;
    InternalLoadPreset(FPresets.Items[FCurPreset].Values);
    Result:=FCurPreset;
  end;
end;

procedure TVPresetBase.CopyPreset;
begin
  if FCurPreset<FNumPreset then
  begin
    FPresets.Items[FCurPreset].CopyParams(InternalStorePreset);
    FClipboard:=FPresets.Items[FCurPreset].Values;
  end;
end;

procedure TVPresetBase.PastePreset;
begin
  if FCurPreset<FNumPreset then
  begin
    FPresets.Items[FCurPreset].CopyParams(FClipboard);
    InternalLoadPreset(FClipboard);
  end;
end;

destructor TVPresetBase.Destroy;
begin
  FPresets.Free;
  inherited Destroy;
end;

function TVPresetBase.GetPresetName:AnsiString;
begin
  if FCurPreset<FNumPreset then
    Result:=FPresets.Items[FCurPreset].Name
  else
    Result:='';
end;

function TVPresetBase.GetPresetNameIndexed(index:Integer;const ptr:PAnsiChar):Integer;
begin
  if (index>=0) and (index<FNumPreset) then
  begin
    VstStrncpy(ptr,FPresets.Items[index].Name,23);
    Result:=1;
  end else Result:=0;
end;

procedure TVPresetBase.SetPreset(index:Integer);
begin
  if (index<>FCurPreset) and (index>=0) and (index<FNumPreset) then
  begin
    InternalLoadPreset(FPresets.Items[index].Values);
    FCurPreset:=index;
  end;
end;

procedure TVPresetBase.SetPresetName(ptr:PAnsiChar);
begin
  if FNumPreset>0 then
    FPresets.Items[FCurPreset].Name:=ptr;
end;

{ TVMidiBase }

procedure TVMidiBase.ProcessMidiEvent(const Event:TVstMidiEvent);
begin
  if Assigned(FOnMidiIn) then
    FOnMidiIn(Event.MidiData[0],Event.MidiData[1],Event.MidiData[2]);
  FMidiStatus.IsNew:=True;
  FMidiStatus.Command:=Event.MidiData[0];
  FMidiStatus.Note:=Event.MidiData[1];
  FMidiStatus.Velocity:=Event.MidiData[2];
end;

function TVMidiBase.QueryNewMidiData(out Command,Note,Velocity:Byte):Boolean;
begin
  Result:=False;
  if FMidiStatus.IsNew then
  begin
    Result:=True;
    Command:=FMidiStatus.Command;
    Note:=FMidiStatus.Note;
    Velocity:=FMidiStatus.Velocity;
    FMidiStatus.IsNew:=False;
  end;
end;

procedure TVMidiBase.SetOnMidiIn(MidiInProc:TOnMidiInProc);
begin
  FOnMidiIn:=MidiInProc;
end;

procedure TVMidiBase.ProcessEvents(Events:PVstEvents);
var
  i:Integer;
  Event:TVstEvent;
begin
  if Assigned(Events) then
    with Events^ do
    for i:=0 to NumEvents-1 do
    begin
      Event:=Events[i]^;
      if Event.Typ=kVstMidiType then
        ProcessMidiEvent(TVstMidiEvent(Event))
      else if Event.Typ=kVstSysExType then
        ;
    end;
end;


end.

