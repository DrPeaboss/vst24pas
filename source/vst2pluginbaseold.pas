{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2pluginbase
// Description : Classes for vst2 plugin (old)
// Created by  : PeaZomboss, 2021/07
-------------------------------------------------------------------------------}

unit vst2pluginbaseold;

{$WARN 5024 off : Parameter "$1" not used}
{$I vcompiler.inc}

interface

uses
  vst2intf,forms,sysutils;

type
  // Vendor specific function for effVendorSpecific and amVendorSpecific
  TVendorSpecific = function(lArg1, lArg2: Int32; ptrArg: Pointer; floatArg: single):IntPtr of object;

  // Effect infomations
  TEffectInfo = record
    EffectName:string;
    Vendor:string;
    Product:string;
    VendorVersion:Integer;
    Category:TVstPlugCategory;
    CanDos:TStringArray; // see CanDo strings in vst2interfaces
  end;

  // The display mode for effGetParamDisplay
  TParamDisplayMode = (pdmNone, pdmCustom, pdmFloat, pdmdB, pdmInteger{, pdmHz, pdmMs});
  // Custom parameter display function if you set pdmCustom
  TCustomParamDisplay = function(index:integer):string of object;
  PParamInfo = ^TParamInfo;
  // Parameter informations
  TParamInfo = record
    Value:single; // The parameter value in range [0,1]
    Name:string; // e.g. Gain, Frequency ...
    Lbl:string; // e.g. dB, % ...
    DisplayMode:TParamDisplayMode; // Set pdmCustom then do it yourself
    CanBeAutomated:boolean; // Whether the value can be automated by host
    Properties:PVstParameterProperties; // Support VstParameterProperties if not nil
  end;
  TParamInfos = array of TParamInfo;

  PPresetInfo = ^TPresetInfo;
  // Preset(program) informations
  TPresetInfo = record
    Name:string; // Preset's name
    Params:array of single; // All parameters
  end;
  TPresetInfos = array of TPresetInfo;

  TPresetChunk = array of single;

  TPanLaw = record
    Gain:single;
    plType:TVstPanLawType;
  end;

  IPluginComponent = interface
    ['{6085C611-5DEC-403F-9AF1-DB47F84C6F79}']
    procedure SetParameter(index:integer;value:single);
    function GetParameter(index:integer):single;
    property Parameters[index:integer]:single read GetParameter write SetParameter;
  end;

  IPluginEditor = interface
    ['{24DF445F-7BDF-4E47-9228-ED67BF916F8D}']
    function GetPlugin:IPluginComponent;
    function GetGui:TForm;
    function IsOpen: boolean;
    procedure SetIdleProc(AProc:TProcedureOfObject);
    property Plugin:IPluginComponent read GetPlugin;
    property Gui:TForm read GetGui;
  end;

  IPluginAudioProcessor = interface
    ['{D417EFD3-E0A2-4599-984D-092152C0102B}']
    procedure Process(const inputs,outputs:TBuffer32;sampleframes:integer);
    procedure Process32(const inputs,outputs:TBuffer32;sampleframes:integer);
{$ifdef VST_2_4_EXTENSIONS}
    procedure Process64(const inputs,outputs:TBuffer64;sampleframes:integer);
{$endif}
  end;

  { TPluginBase }

  // Plugin base class that disable reference count
  TPluginBase = class(TObject,IUnknown)
  protected
    function QueryInterface({$ifdef FPC}{$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
      {$else}const{$endif}iid: tguid; out obj): longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
    function _AddRef: longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
    function _Release: longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
  end;

  TPluginEditor = class;

  { TPluginComponent }

  // Plugin component without process
  TPluginComponent = class(TPluginBase, IPluginComponent)
  private
    FHost:THostCallback;
    FEffect:TAEffect;
    FEditor:TPluginEditor;
    FSampleRate:single;
    FBlockSize:Int32;
    FNumPrograms:Int32;
    FNumParams:Int32;
    FCurProgram:Int32;
    FEffectInfo:TEffectInfo;
    FParamInfos:TParamInfos;
    FPresetInfos:TPresetInfos;
    FPresetChunk:TPresetChunk;
    FPanLaw:TPanLaw;
    FProcessPrecision:TVstProcessPrecision;
    // Events
    FEffOpen:TProcedureOfObject;
    FEffClose:TProcedureOfObject;
    FEffTurnOn:TProcedureOfObject;
    FEffTurnOff:TProcedureOfObject;
    FEffVendorSpecific:TVendorSpecific;
    FEffSetPreset:TProcedureOfObject;
    FEffEndSetPreset:TProcedureOfObject;
    FEffStartProcess:TProcedureOfObject;
    FEffStopProcess:TProcedureOfObject;
    FCustomParamDisplay:TCustomParamDisplay;
    function GetAEffect:PAEffect;
    function GetEditor:IPluginEditor;
    procedure SetNumParams(Num:Int32);
    procedure SetNumPrograms(Num:Int32);
  protected
    function Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer; opt: single):IntPtr;virtual;
    procedure PlugInitEffectInfo(Name:string='';Vendor:string='';Product:string='';VendorVersion:int32=0;
      Category:TVstPlugCategory=kPlugCategEffect;CanDos:TStringArray=nil);
    procedure PlugInitParamInfo(index:int32;value:single=0;name:string='';lbl:string='';
      dispmode:TParamDisplayMode=pdmFloat;CanBeAutomated:boolean=true);
    procedure PlugInitPreset(index:int32;const Name:string;values:array of single);
    // Float to string, max length is 7 due to bad precision when parameter type is single
    function Float2String(const value:double):shortstring;
    // Integer part of float to string, max length is 7, reason as above
    function Int2String(const value:double):string;
    // Set number of inputs and outputs, pass -1 to ignore one
    procedure SetNumberInOut(InNum:Int32=2;OutNum:Int32=2);
    // Set your plugin's editor with lcl or vcl form
    procedure SetEditor(Editor:TPluginEditor);overload;
    procedure SetEditor(GuiClass:TFormClass);overload;
    // Set your plugin's unique ID, must be called
    procedure SetUniqueID(A,B,C,D:AnsiChar);overload;
    procedure SetUniqueID(ID:Integer);overload;
    // Set your plugin's version
    procedure SetVersion(Version:Int32);
    // Switch a flag in FEffect.Flags
    procedure SetEffectFlag(flag:TVstAEffectFlag;state:boolean=true);
    // Set value of a parameter by index
    // Warnning: you should set Parameters[index]:=value only once
    // Because this procedure called host callback function, see implement for more info
    procedure SetParameter(index: integer; value: single);
    // Get value of a parameter by index
    function GetParameter(index: integer): single;
    // Used for effCanDo, see TPcdStrings strings, check strings in FEffectInfo.CanDos
    function CanDo(str:PAnsiChar):integer;
    // Used for effGetParamDisplay
    function GetParamDisplay(index:integer):string;
  public
    constructor Create(VstHost: THostCallback); virtual;
    destructor Destroy; override;
    // Carefully using it, see SetParameter to get more information
    property Parameters[index:integer]:single read GetParameter write SetParameter;
    property Editor:IPluginEditor read GetEditor;
    property AEffect:PAEffect read GetAEffect;
    // Can call host directly for advanced usage
    property VHost:THostCallback read FHost;
    // effOpen
    property OnEffOpen:TProcedureOfObject write FEffOpen;
    // effClose
    property OnEffClose:TProcedureOfObject write FEffClose;
    // effMainsChanged when value is 1 (turn on)
    property OnEffTrunOn:TProcedureOfObject write FEffTurnOn;
    // effMainsChanged when value is 0 (turn off)
    property OnEffTrunOff:TProcedureOfObject write FEffTurnOff;
    // effVendorSpecific
    property OnEffVendorSpecific:TVendorSpecific write FEffVendorSpecific;
    // effBeginSetProgram
    property OnEffSetPreset:TProcedureOfObject write FEffSetPreset;
    // effEndSetProgram
    property OnEffEndSetPreset:TProcedureOfObject write FEffEndSetPreset;
    // effStartProcess
    property OnEffStartProcess:TProcedureOfObject write FEffStartProcess;
    // effStopProcess
    property OnEffStopProcess:TProcedureOfObject write FEffStopProcess;
    // Set it if you set pdmCustom of your parameter, function see TCustomParamDisplay
    property OnCustomParamDisplay:TCustomParamDisplay write FCustomParamDisplay;
  end;

  { TPluginEditor }

  // Plugin editor using lcl or vcl forms
  TPluginEditor = class(TPluginBase, IPluginEditor)
  private
    FPlugin:TPluginComponent;
    FIsOpen:Boolean;
    FGui:TForm;
    FRect:TERect;
    FIdleProc:TProcedureOfObject;
    function GetGui:TForm;
    function GetPlugin:IPluginComponent;
  protected
    function GetRect(rect:PPERect):longint;
    function Open(ptr: Pointer):longint;
    procedure Close;
    procedure SetIdleProc(AProc:TProcedureOfObject);
    function IsOpen: boolean;
{$ifdef VST_2_1_EXTENSIONS}
    function OnKeyDown(var keyCode: TVstKeyCode): longint;virtual;
    function OnKeyUp(var keyCode: TVstKeyCode): longint;virtual;
    function OnWheel(distance: single): longint;virtual;
    function SetKnobMode(val: Int32):longint;virtual;
{$endif VST_2_1_EXTENSIONS}
  public
    constructor Create(Plugin:TPluginComponent;GuiClass:TFormClass); virtual;
    destructor Destroy; override;
    property Gui:TForm read GetGui;
    property Plugin:IPluginComponent read GetPlugin;
  end;

  TVSTPluginClass = class of TVSTPlugin;

  { TVSTPlugin }

  // The plugin class with full functions
  TVSTPlugin = class(TPluginComponent, IPluginAudioProcessor)
  private

  protected
    function Dispatcher(opcode:TAEffectOpcodes;index:Int32;Value:IntPtr;ptr:Pointer;opt:single):IntPtr;override;
    // IPluginAudioProcessor
    // Must be implemented in subclass if version before 2.4
    procedure Process(const inputs,outputs:TBuffer32;sampleframes:integer);virtual;{$ifndef VST_2_4_EXTENSIONS}abstract;{$endif}
    // Must be implemented in subclass if version is 2.4
    procedure Process32(const inputs,outputs:TBuffer32;sampleframes:integer);virtual;{$ifdef VST_2_4_EXTENSIONS}abstract;{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    // Should set flag effFlagsCanDoubleReplacing first to use it
    // You can call EnableProcess64 to quickly set it
    procedure Process64(const inputs, outputs: TBuffer64; sampleframes: integer);virtual;
{$endif}
  public
    constructor Create(VstHost: THostCallback); override;
    destructor Destroy; override;
{$ifdef VST_2_4_EXTENSIONS}
    procedure EnableProcess64(state:boolean=true);
{$endif}
  end;

// Here are callback functions used in AEffect

function DispatchEffectCb(e: PAEffect; opcode, index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
function GetParameterCb(e: PAEffect; index: Int32): single; cdecl;
procedure SetParameterCb(e: PAEffect; index: Int32; value: single); cdecl;
procedure ProcessCb(e: PAEffect; inputs, outputs: PPSingle; sampleFrames: Int32); cdecl;
procedure Process32Cb(e: PAEffect; inputs, outputs: PPSingle; sampleFrames: Int32); cdecl;
{$ifdef VST_2_4_EXTENSIONS}
procedure Process64Cb(e: PAEffect; inputs, outputs: PPDouble; sampleFrames: Int32); cdecl;
{$endif}

// A helper function to quickly create exported function VSTPluginMain, usage:
{-------------------------------------------------------------------------------
function VSTPluginMain(VstHost: TVstHostCallback): PAEffect; cdecl; export;
begin
  Result := DoVSTPluginMain(VstHost, TMyPlugin);
end;
--------------------------------------------------------------------------------
Note: TMyPlugin is your plugin class }
function DoVSTPluginMain(VstHost: THostCallback; PluginClass:TVSTPluginClass):PAEffect;
// A helper function to convert the amplitude to decibels, value should bigger than 1E-7
function VstAmp2dB(const value:double):double;inline;
// A helper function to convert the decibels to amplitude, value should bigger than -140
function VstdB2Amp(const value:double):double;inline;

implementation

uses
  Controls,Math;

function DispatchEffectCb(e: PAEffect; opcode, index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr; cdecl;
var
  v:TPluginComponent;
begin
  v:=TPluginComponent(e^.pObject);
  if opcode=ord(effClose) then
  begin
    v.Dispatcher(TAEffectOpcodes(opcode),index,value,ptr,opt);
    v.Free;
    Exit(1);
  end;
  Result:=v.Dispatcher(TAEffectOpcodes(opcode),index,value,ptr,opt);
end;

function GetParameterCb(e: PAEffect; index: Int32): single; cdecl;
begin
  Result:=TPluginComponent(e^.pObject).GetParameter(index);
end;

procedure SetParameterCb(e: PAEffect; index: Int32; value: single); cdecl;
begin
  // Simply set the value, see TPluginComponent.SetParameter to get the reason
  TPluginComponent(e^.pObject).FParamInfos[index].Value := value;
end;

procedure ProcessCb(e: PAEffect; inputs, outputs: PPSingle; sampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVSTPlugin(e^.pObject).Process(inputs,outputs,sampleframes);
end;
{$else}
var
  i: integer;
  InputsArr, OutputsArr: TArrPSingle;
begin
  SetLength(InputsArr, e^.NumInputs);
  SetLength(OutputsArr, e^.NumOutputs);
  for i := 0 to e^.NumInputs - 1 do
  begin
    InputsArr[i] := Pointer(inputs^);
    Inc(inputs);
  end;
  for i := 0 to e^.NumOutputs - 1 do
  begin
    OutputsArr[i] := Pointer(outputs^);
    Inc(outputs);
  end;
  TVSTPlugin(e^.pObject).Process(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}

procedure Process32Cb(e: PAEffect; inputs, outputs: PPSingle; sampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVSTPlugin(e^.pObject).Process32(inputs,outputs,sampleframes);
end;
{$else}
var
  i: integer;
  InputsArr, OutputsArr: TArrPSingle;
begin
  SetLength(InputsArr, e^.NumInputs);
  SetLength(OutputsArr, e^.NumOutputs);
  for i := 0 to e^.NumInputs - 1 do
  begin
    InputsArr[i] := Pointer(inputs^);
    Inc(inputs);
  end;
  for i := 0 to e^.NumOutputs - 1 do
  begin
    OutputsArr[i] := Pointer(outputs^);
    Inc(outputs);
  end;
  TVSTPlugin(e^.pObject).Process32(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}

{$ifdef VST_2_4_EXTENSIONS}
procedure Process64Cb(e: PAEffect; inputs, outputs: PPDouble; sampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVSTPlugin(e^.pObject).Process64(inputs,outputs,sampleframes);
end;
{$else}
var
  i: integer;
  InputsArr, OutputsArr: TArrPDouble;
begin
  SetLength(InputsArr, e^.NumInputs);
  SetLength(OutputsArr, e^.NumOutputs);
  for i := 0 to e^.NumInputs - 1 do
  begin
    InputsArr[i] := Pointer(inputs^);
    Inc(inputs);
  end;
  for i := 0 to e^.NumOutputs - 1 do
  begin
    OutputsArr[i] := Pointer(outputs^);
    Inc(outputs);
  end;
  TVSTPlugin(e^.pObject).Process64(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}
{$endif}

function DoVSTPluginMain(VstHost: THostCallback; PluginClass: TVSTPluginClass): PAEffect;
var
  Plugin:TVSTPlugin;
begin
  Plugin:=PluginClass.Create(VstHost);
  Result:=Plugin.AEffect;
end;

function VstAmp2dB(const value: double): double;
begin
  if value>=1E-7 then
    Result:=20*Log10(value)
  else
    Result:=NegInfinity;
end;

function VstdB2Amp(const value: double): double;
begin
  if value >= -140 then
    Result:=exp(value*0.1151292546497)
  else
    Result:=0;
end;

{ TVSTPlugin }

constructor TVSTPlugin.Create(VstHost: THostCallback);
begin
  inherited Create(VstHost);
end;

destructor TVSTPlugin.Destroy;
begin
  inherited Destroy;
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVSTPlugin.EnableProcess64(state: boolean);
begin
  SetEffectFlag(effFlagsCanDoubleReplacing,state);
end;
{$endif}

function TVSTPlugin.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr;
begin
  case opcode of
    // TODO
    effOpen:;
    effClose:;
    effMainsChanged:;
    else Result := inherited Dispatcher(opcode, index, Value, ptr, opt);
  end;
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVSTPlugin.Process(const inputs, outputs: TBuffer32; sampleframes: integer);
begin
end;

procedure TVSTPlugin.Process64(const inputs, outputs: TBuffer64; sampleframes: integer);
begin
end;
{$else}
procedure TVSTPlugin.Process32(const inputs, outputs: TBuffer32; sampleframes: integer);
begin
end;
{$endif}

{ TPluginBase }

function TPluginBase.QueryInterface({$ifdef FPC}{$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF}
  {$else}const{$endif}iid: tguid; out obj): longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
begin
  if GetInterface(iid,obj) then
    Result:=S_OK
  else
    Result:=E_NOINTERFACE;
end;

function TPluginBase._AddRef: longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
begin
  Result:=-1;
end;

function TPluginBase._Release: longint;{$ifdef MSWINDOWS}stdcall;{$else}cdecl;{$endif}
begin
  Result:=-1;
end;

{ TPluginEditor }

constructor TPluginEditor.Create(Plugin: TPluginComponent; GuiClass: TFormClass);
begin
  FPlugin:=Plugin;
  FGui:=GuiClass.Create(nil);
  FGui.Left:=0;
  FGui.Top := 0;
  FGui.BorderStyle:=bsNone;
  FIsOpen:=False;
end;

destructor TPluginEditor.Destroy;
begin
  FIsOpen:=False;
  FGui.Free;
  inherited Destroy;
end;

function TPluginEditor.GetGui: TForm;
begin
  Result:=FGui;
end;

function TPluginEditor.GetPlugin: IPluginComponent;
begin
  Result:=FPlugin;
end;

function TPluginEditor.GetRect(rect: PPERect): longint;
begin
  FRect.Left := 0;
  FRect.Top := 0;
  FRect.Right := FGui.Width;
  FRect.Bottom := FGui.Height;
  rect^:=@FRect;
  Result:=1;
end;

function TPluginEditor.Open(ptr: Pointer): longint;
begin
  FGui.ParentWindow := ToIntPtr(ptr);
  FGui.Show;
  FIsOpen:=True;
  Result := 1;
end;

procedure TPluginEditor.Close;
begin
  FGui.Hide;
  FGui.ParentWindow:=0;
  FIsOpen:=False;
end;

function TPluginEditor.IsOpen: boolean;
begin
  Result:=FIsOpen;
end;

procedure TPluginEditor.SetIdleProc(AProc: TProcedureOfObject);
begin
  FIdleProc := AProc;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TPluginEditor.OnKeyDown(var keyCode: TVstKeyCode): longint;
begin
  Result:=0;
end;

function TPluginEditor.OnKeyUp(var keyCode: TVstKeyCode): longint;
begin
  Result:=0;
end;

function TPluginEditor.OnWheel(distance: single): longint;
begin
  Result:=0;
end;

function TPluginEditor.SetKnobMode(val: Int32): longint;
begin
  Result:=0;
end;
{$endif}

{ TPluginComponent }

constructor TPluginComponent.Create(VstHost: THostCallback);
begin
  FHost := VstHost;
  FNumPrograms := 0;
  FNumParams := 0;
  FEditor  := nil;
  FSampleRate := 44100;
  FBlockSize := 1024;
  FCurProgram := 0;
  FEffect.Magic := kEffectMagic;
  FEffect.Dispatcher := @DispatchEffectCb;
  FEffect.Process := @ProcessCb; // Though is deprecated
  FEffect.SetParameter := @SetParameterCb;
  FEffect.GetParameter := @GetParameterCb;
  FEffect.NumPrograms := FNumPrograms;
  FEffect.NumParams := FNumParams;
  FEffect.NumInputs := 2;  // stereo input
  FEffect.NumOutputs := 2; // stereo output
  FEffect.IORatio := 1;
  FEffect.pObject := self;
  FEffect.UniqueID := MakeLong('N', 'o', 'E', 'f');
  FEffect.Version := 1;
  FEffect.ProcessReplacing := @Process32Cb;
{$ifdef VST_2_4_EXTENSIONS}
  SetEffectFlag(effFlagsCanReplacing); // mandatory in VST 2.4!
  FEffect.ProcessDoubleReplacing := @Process64Cb;
{$endif}
  SetEffectFlag(effFlagsProgramChunks);
end;

destructor TPluginComponent.Destroy;
var
  i:integer;
begin
  if (effFlagsHasEditor in FEffect.Flags) and Assigned(FEditor) then FEditor.Free;
  if FNumPrograms>0 then
    for i:=0 to FNumPrograms-1 do
      SetLength(FPresetInfos[i].Params,0);
  SetLength(FPresetInfos,0);
  SetLength(FParamInfos,0);
  SetLength(FPresetChunk,0);
  inherited Destroy;
end;

function TPluginComponent.CanDo(str: PAnsiChar): integer;
var
  s:string;
begin
  Result:=-1;
  for s in FEffectInfo.CanDos do
    if s=StrPas(str) then Exit(1);
end;

function TPluginComponent.GetParamDisplay(index: integer): string;
begin
  Result:='';
  if index<FNumParams then
  with FParamInfos[index] do
  case DisplayMode of
    pdmFloat:Result:=Float2String(Value);
    pdmdB:Result:=Float2String(VstAmp2dB(Value));
    pdmInteger:Result:=Int2String(Value);
    // Not sure
    //pdmMs:Result:=Float2String(Value*1000/FSampleRate);
    //pdmHz:if Value=0 then Result:='0' else Result:=Float2String(FSampleRate*Value*0.5);
    pdmCustom:if Assigned(FCustomParamDisplay) then Result:=FCustomParamDisplay(index);
    else ;
  end;
end;

function TPluginComponent.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr;
var
  i: Integer;
{$ifdef VST_2_1_EXTENSIONS}
  KeyCode: TVstKeyCode;
{$endif}
begin
  Result:=0;
  case opcode of
    effOpen: if Assigned(FEffOpen) then FEffOpen;
    effClose: if Assigned(FEffClose) then FEffClose;
    effSetProgram: if value<FNumPrograms then
                   begin
                     FCurProgram := value;
                     for i:=0 to FNumParams-1 do
                       FParamInfos[i].Value:=FPresetInfos[FCurProgram].Params[i];
                   end;
    effGetProgram: Result:=FCurProgram;
    effSetProgramName: FPresetInfos[FCurProgram].Name := StrPas(ptr);
    effGetProgramName: VstStrncpy(ptr,PAnsiChar(FPresetInfos[FCurProgram].Name),23);
    effGetParamLabel: VstStrncpy(ptr,PAnsiChar(FParamInfos[index].Lbl),7);
    effGetParamDisplay: VstStrncpy(ptr,PAnsiChar(GetParamDisplay(index)),15);
    effGetParamName: VstStrncpy(ptr,PAnsiChar(FParamInfos[index].Name),15);
{$ifndef VST_FORCE_DEPRECATED}
    effGetVu:;
{$endif}
    effSetSampleRate: FSampleRate := opt;
    effSetBlockSize: FBlockSize := Value;
    effMainsChanged: if value=1 then begin
                       if (CanDo(TPcdStrings.cdReceiveVstMidiEvent)=1) or
                          (effFlagsIsSynth in FEffect.Flags) then
                          FHost(@FEffect,ord(amWantMidi),0,1,nil,0);
                       if Assigned(FEffTurnOn) then FEffTurnOn;
                     end else if Assigned(FEffTurnOff) then FEffTurnOff;
    effEditGetRect: Result:=FEditor.GetRect(ptr);
    effEditOpen: Result:=FEditor.Open(ptr);
    effEditClose: FEditor.Close;
    effEditIdle: if Assigned(FEditor.FIdleProc) then FEditor.FIdleProc;
    effIdentify: Result:=FEffect.UniqueID; // deprecated
    effGetChunk: begin
                   for i:=0 to FNumParams-1 do
                     FPresetChunk[i]:=Parameters[i];
                   PPointer(ptr)^:=FPresetChunk;
                   Result:=FNumParams*sizeof(single);
                 end;
    effSetChunk: begin
                   for i:=0 to FNumParams-1 do begin
                     Parameters[i]:=PSingle(ptr)^;
                     inc(PSingle(ptr));
                   end;
                   Result:=1;
                 end;
    effProcessEvents: ;
    effCanBeAutomated: Result:=IntPtr(FParamInfos[index].CanBeAutomated);
    effString2Parameter: Result:=IntPtr(TryStrToFloat(StrPas(ptr),FParamInfos[index].Value));
    effGetProgramNameIndexed: if index<FNumPrograms then begin
                                VstStrncpy(ptr,PAnsiChar(FPresetInfos[index].Name),23);
                                Result:=1;
                              end;
{$ifndef VST_FORCE_DEPRECATED}
    effGetNumProgramCategories: Result:=1;
    effCopyProgram: Result:=0;
    effConnectInput: Result:=1;
    effConnectOutput: Result:=1;
{$endif}
    effGetInputProperties: ;
    effGetOutputProperties: ;
    effGetPlugCategory: if effFlagsIsSynth in FEffect.Flags then Result:=IntPtr(kPlugCategSynth)
                        else Result:=IntPtr(FEffectInfo.Category);
{$ifndef VST_FORCE_DEPRECATED}
    effGetCurrentPosition: Result:=0;
    effGetDestinationBuffer: Result:=0;
{$endif}
    effOfflineNotify: ;
    effOfflinePrepare: ;
    effOfflineRun: ;
    effProcessVarIO: ;
    effSetSpeakerArrangement: ;
{$ifndef VST_FORCE_DEPRECATED}
    effSetBlockSizeAndSampleRate: begin FBlockSize:=value; FSampleRate:=opt; end;
{$endif}
    effSetBypass: ;
    effGetEffectName:begin VstStrncpy(ptr,PAnsiChar(FEffectInfo.EffectName),31); Result:=1; end;
    effGetVendorString:begin VstStrncpy(ptr,PAnsiChar(FEffectInfo.Vendor),63); Result:=1; end;
    effGetProductString:begin VstStrncpy(ptr,PAnsiChar(FEffectInfo.Product),63); Result:=1; end;
    effGetVendorVersion: Result:=FEffectInfo.VendorVersion;
    effVendorSpecific:if Assigned(FEffVendorSpecific) then Result:=FEffVendorSpecific(index,Value,ptr,opt);
    effCanDo: Result:=CanDo(ptr);
    effGetTailSize: ;
{$ifndef VST_FORCE_DEPRECATED}
    effGetErrorText: ;
    effGetIcon: ;
    effSetViewPosition: ;
    effIdle: ;
    effKeysRequired: ;
{$endif}
    effGetParameterProperties: if Assigned(FParamInfos[index].Properties) then
                               begin
                                 PVstParameterProperties(ptr)^:=FParamInfos[index].Properties^;
                                 Result:=1;
                               end else Result:=0;
    effGetVstVersion: Result:=kVstVersion;
{$ifdef VST_2_1_EXTENSIONS}
    effEditKeyDown: begin
                      KeyCode.Character := index;
                      KeyCode.modifier := TVstModifierKeys(Value);
                      KeyCode.virt := TVstVirtualKey(Trunc(opt));
                      Result := FEditor.OnKeyDown(KeyCode);
                    end;
    effEditKeyUp: begin
                    KeyCode.Character := index;
                    KeyCode.modifier := TVstModifierKeys(Value);
                    KeyCode.virt := TVstVirtualKey(Trunc(opt));
                    Result := FEditor.OnKeyUp(KeyCode);
                  end;
    effSetEditKnobMode: Result:=FEditor.SetKnobMode(Value);
    effGetMidiProgramName: ;
    effGetCurrentMidiProgram: Result:=-1;
    effGetMidiProgramCategory: ;
    effHasMidiProgramsChanged: ;
    effGetMidiKeyName: ;
    effBeginSetProgram:if Assigned(FEffSetPreset) then FEffSetPreset;
    effEndSetProgram:if Assigned(FEffEndSetPreset) then FEffEndSetPreset;
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement: ;
    effShellGetNextPlugin: ; // Only called if is kPlugCategShell
    effStartProcess:if Assigned(FEffStartProcess) then FEffStartProcess;
    effStopProcess:if Assigned(FEffStopProcess) then FEffStopProcess;
    effSetTotalSampleToProcess: Result:=Value;
    effSetPanLaw: begin FPanLaw.plType := TVstPanLawType(value); FPanLaw.Gain := opt; end;
    effBeginLoadBank: ;
    effBeginLoadProgram: ;
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision: FProcessPrecision:=TVstProcessPrecision(value);
    effGetNumMidiInputChannels: ;
    effGetNumMidiOutputChannels: ;
{$endif}
    else;
  end;
end;

procedure TPluginComponent.SetParameter(index: integer; value: single);
begin
  if index<FNumParams then
  begin
    FParamInfos[index].Value:=value;
    if FCurProgram>0 then FPresetInfos[FCurProgram].Params[index]:=value;
    if FParamInfos[index].CanBeAutomated then
      FHost(@FEffect,ord(amAutomate),index,0,nil,value); // Can be called only once !
      // This will tell host this parameter can be automated
      // If you called it twice, the automation will stop
  end;
end;

function TPluginComponent.GetParameter(index: integer): single;
begin
  if index<FNumParams then Result:=FParamInfos[index].Value;
end;

function TPluginComponent.GetAEffect: PAEffect;
begin
  Result:=@FEffect;
end;

function TPluginComponent.GetEditor: IPluginEditor;
begin
  Result:=FEditor as IPluginEditor;
end;

procedure TPluginComponent.SetNumParams(Num: Int32);
begin
  FNumParams := Num;
  FEffect.NumParams := Num;
end;

procedure TPluginComponent.SetNumPrograms(Num: Int32);
begin
  FNumPrograms := Num;
  FEffect.NumPrograms := Num;
end;

procedure TPluginComponent.PlugInitEffectInfo(Name: string; Vendor: string; Product: string; VendorVersion: int32;
  Category: TVstPlugCategory; CanDos: TStringArray);
begin
  FEffectInfo.EffectName := Name;
  FEffectInfo.Vendor := Vendor;
  FEffectInfo.Product := Product;
  FEffectInfo.VendorVersion := VendorVersion;
  FEffectInfo.Category := Category;
  FEffectInfo.CanDos := CanDos;
end;

procedure TPluginComponent.PlugInitParamInfo(index: int32; value: single; name: string; lbl: string;
  dispmode: TParamDisplayMode; CanBeAutomated: boolean);
begin
  //if FNumPrograms<>0 then raise Exception.Create('Cannot init param after preset');
  //if index<>FNumParams then raise Exception.Create('Must zero based and continuous');
  if (FNumPrograms<>0) or (index<>FNumParams) then Exit;
  SetNumParams(index+1);
  SetLength(FParamInfos, FNumParams);
  FParamInfos[index].Value := value;
  FParamInfos[index].Name := name;
  FParamInfos[index].Lbl := lbl;
  FParamInfos[index].DisplayMode := dispmode;
  FParamInfos[index].CanBeAutomated := CanBeAutomated;
  SetLength(FPresetChunk, FNumParams);
end;

procedure TPluginComponent.PlugInitPreset(index: int32; const Name: string; values: array of single);
var
  i: Integer;
begin
  //if index<>FNumPrograms then raise Exception.Create('Must zero based and continuous');
  //if length(values)<>FNumParams then raise Exception.Create('Parameter numbers unequal');
  if (index<>FNumPrograms) or (Length(values)<>FNumParams) then Exit;
  SetNumPrograms(index+1);
  SetLength(FPresetInfos, FNumPrograms);
  SetLength(FPresetInfos[index].Params, FNumParams);
  FPresetInfos[index].Name := Name;
  for i:=0 to FNumParams-1 do
    FPresetInfos[index].Params[i]:=values[i];
end;

function TPluginComponent.Float2String(const value: double): shortstring;
var
  i: integer;
  mantissa: double;
begin
  if value=NegInfinity then Exit('-Inf'); // Cooperate with VstAmp2dB
  if (Value > 999999) or (Value < -99999) then
    Exit('Huge !!');
  Result := IntToStr(Trunc(Value));
  mantissa := Abs(Frac(Value));
  if Length(Result) = 6 then
  begin
    // If the param type is *single*, 999998.47, 999998.48 and 999998.49
    // will be considered as 999998.5 due to precision, so the final result
    // will be 999999 rather 999998
    // Actually, 999998.47 to 999998.53 will all be 999998.5
    if mantissa>=0.5 then Result:=Float2String(Int(Value)+1);
    Exit; // No dot at last place if length is 6
  end;
  Result := Result + '.';
  i := Length(Result);
  while i<=6 do
  begin
    mantissa := Frac(mantissa) * 10;
    Result := Result + AnsiChar(Trunc(mantissa) + 48);
    Inc(i);
  end;
  mantissa := Frac(mantissa) * 10;
  if mantissa >= 5 then // Similar reason, see above
  begin
    while Result[i] = '9' do
    begin
      Result[i] := '0';
      Dec(i);
    end;
    if Result[i] <> '.' then
      Inc(Result[i])
    else
      Result := Float2String(Int(Value)+1);
  end;
end;

function TPluginComponent.Int2String(const value: double): string;
begin
  if (value>9999999) or (value<-999999) then
    Exit('Huge !!');
  Result:=IntToStr(Trunc(value));
end;

procedure TPluginComponent.SetNumberInOut(InNum: Int32; OutNum: Int32);
begin
  if InNum>-1 then FEffect.NumInputs := InNum;
  if OutNum>-1 then FEffect.NumOutputs := OutNum;
end;

procedure TPluginComponent.SetEditor(Editor: TPluginEditor);
begin
  if Assigned(Editor) then begin
    FEditor:=Editor;
    Include(FEffect.Flags,effFlagsHasEditor);
  end;
end;

procedure TPluginComponent.SetEditor(GuiClass: TFormClass);
begin
  SetEditor(TPluginEditor.Create(self,GuiClass));
end;

procedure TPluginComponent.SetUniqueID(A, B, C, D: AnsiChar);
begin
  FEffect.UniqueID := MakeLong(A,B,C,D);
end;

procedure TPluginComponent.SetUniqueID(ID: Integer);
begin
  FEffect.UniqueID := ID;
end;

procedure TPluginComponent.SetVersion(Version: Int32);
begin
  FEffect.Version := Version;
end;

procedure TPluginComponent.SetEffectFlag(flag: TVstAEffectFlag; state: boolean);
begin
  if state then
    Include(FEffect.Flags,flag)
  else
    Exclude(FEffect.Flags,flag);
end;

end.

