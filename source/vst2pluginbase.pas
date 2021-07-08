{-------------------------------------------------------------------------------
//
// Unit name   : vst2pluginbase
// Description : Classes for vst2 plugin
// Created by  : PeaZomboss, 2021/07
-------------------------------------------------------------------------------}
unit vst2pluginbase;

{$I vcompiler.inc}

interface

uses
  vst2interfaces,forms,sysutils;

type
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
  TParamDisplayMode = (pdmFloat, pdmdB, pdmInteger, pdmHz, pdmMs, pdmCustom);
  PParamInfo = ^TParamInfo;
  // Parameter informations
  TParamInfo = record
    Value:single; // The parameter value in range [0,1]
    Name:string; // e.g. Gain, Delay, Ratio ...
    Lbl:string; // e.g. dB, % ...
    DisplayMode:TParamDisplayMode; // Set pdmCustom then do it yourself or do nothing
    CanBeAutomated:boolean; // Whether the value can be automated by host
    Properties:PVstParameterProperties; // Support VstParameterProperties if not nil
  end;
  TParamInfos = array of TParamInfo;

  PPresetInfo = ^TPresetInfo;
  // Preset(program) informations
  TPresetInfo = record
    Name:string; // Preset name
    Params:array of single; // All parameters
  end;
  TPresetInfos = array of TPresetInfo;

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
    function QueryInterface(constref iid: tguid; out obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  end;

  TPluginEditor = class;

  { TPluginComponent }

  // Plugin component without process
  TPluginComponent = class(TPluginBase, IPluginComponent)
  private
    FHost:TVstHostCallback;
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
    // Set number of inputs and outputs
    procedure SetNumberInOut(InNum:Int32=2;OutNum:Int32=2);
    // Set your plugin's editor with lcl or vcl form
    procedure SetEditor(Editor: TPluginEditor);
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
    // Used for effCanDo, see PlugCanDo strings, or override it
    function CanDo(str:PAnsiChar):integer;virtual;
    // Used for effGetParamDisplay, override it if you set pdmCustom
    function GetParamDisplay(index:integer):string;virtual;
  public
    constructor Create(VstHost: TVstHostCallback); virtual;
    destructor Destroy; override;
    // Change to a preset prepared first
    procedure SetProgram(index:int32);
    // Carefully using it, see SetParameter to get more information
    property Parameters[index:integer]:single read GetParameter write SetParameter;
    property Editor:IPluginEditor read GetEditor;
    property AEffect:PAEffect read GetAEffect;
  end;

  { TPluginEditor }

  // Plugin editor using lcl or vcl forms
  TPluginEditor = class(TPluginBase, IPluginEditor)
  private
    FPlugin:TPluginComponent;
    FParent:Pointer;
    FGui:TForm;
    FRect:TERect;
    function GetGui:TForm;
    function GetPlugin:IPluginComponent;
  protected
    // IPluginEditor
    function GetRect(rect:PPERect):boolean;
    function Open(ptr: Pointer): boolean;
    procedure Close;
    function IsOpen: boolean;
    procedure Idle;virtual;
{$ifdef VST_2_1_EXTENSIONS}
    function OnKeyDown(var keyCode: TVstKeyCode): boolean;virtual;
    function OnKeyUp(var keyCode: TVstKeyCode): boolean;virtual;
    function OnWheel(distance: single): boolean;virtual;
    function SetKnobMode(val: Int32):boolean;virtual;
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
    FOnOpen:TProcedureOfObject;
    FOnClose:TProcedureOfObject;
  protected
    function Dispatcher(opcode:TAEffectOpcodes;index:Int32;Value:IntPtr;ptr:Pointer;opt:single):IntPtr;override;
  public
    constructor Create(VstHost: TVstHostCallback); override;
    destructor Destroy; override;
    procedure EnableProcess64(state:boolean=true);
    // IPluginAudioProcessor
    // Must be implemented in subclass if not 2.4
    procedure Process(const inputs,outputs:TBuffer32;sampleframes:integer);virtual;{$ifndef VST_2_4_EXTENSIONS}abstract;{$endif}
    // Must be implemented in subclass if 2.4
    procedure Process32(const inputs,outputs:TBuffer32;sampleframes:integer);virtual;{$ifdef VST_2_4_EXTENSIONS}abstract;{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    // Should set flag effFlagsCanDoubleReplacing first to use it
    // You can call EnableProcess64 to quickly set it
    procedure Process64(const inputs, outputs: TBuffer64; sampleframes: integer);virtual;
{$endif}
  end;

// Here are callback functions used in AEffect

function DispatchEffectCb(e: PAEffect; opcode: TAEffectOpcodes; index: Int32;
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
function DoVSTPluginMain(VstHost: TVstHostCallback; PluginClass:TVSTPluginClass):PAEffect;

implementation

uses
  Controls,Math;

function DispatchEffectCb(e: PAEffect; opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr; cdecl;
var
  v:TPluginComponent;
begin
  v:=TPluginComponent(e^.pObject);
  if opcode=effClose then
  begin
    v.Dispatcher(opcode,index,value,ptr,opt);
    v.Free;
    Exit(1);
  end;
  Result:=v.Dispatcher(opcode,index,value,ptr,opt);
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
  TCustomPlugin(e^._Object).Process(InputsArr, OutputsArr, SampleFrames);
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
  TCustomPlugin(e^._Object).Process32(InputsArr, OutputsArr, SampleFrames);
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
  TCustomPlugin(e^._Object).Process64(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}
{$endif}

function DoVSTPluginMain(VstHost: TVstHostCallback; PluginClass: TVSTPluginClass): PAEffect;
var
  Plugin:TVSTPlugin;
begin
  Plugin:=PluginClass.Create(VstHost);
  Result:=Plugin.AEffect;
end;

{ TVSTPlugin }

constructor TVSTPlugin.Create(VstHost: TVstHostCallback);
begin
  inherited Create(VstHost);
end;

destructor TVSTPlugin.Destroy;
begin
  inherited Destroy;
end;

procedure TVSTPlugin.EnableProcess64(state: boolean);
begin
  SetEffectFlag(effFlagsCanDoubleReplacing,state);
end;

function TVSTPlugin.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr;
begin
  case opcode of
    // TODO
    effOpen:;
    effClose:;
    else Result := inherited Dispatcher(opcode, index, Value, ptr, opt);
  end;
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVSTPlugin.Process(const inputs, outputs: TBuffer32; sampleframes: integer);
begin
end;
{$else}
procedure TCustomPlugin.Process32(const inputs, outputs: TBuffer32; sampleframes: integer);
begin
end;
{$endif}

procedure TVSTPlugin.Process64(const inputs, outputs: TBuffer64; sampleframes: integer);
begin
end;

{ TPluginBase }

function TPluginBase.QueryInterface(constref iid: tguid; out obj): longint; stdcall;
begin
  if GetInterface(iid,obj) then
    Result:=S_OK
  else
    Result:=E_NOINTERFACE;
end;

function TPluginBase._AddRef: longint; stdcall;
begin
  Result:=-1;
end;

function TPluginBase._Release: longint; stdcall;
begin
  Result:=-1;
end;

{ TPluginEditor }

constructor TPluginEditor.Create(Plugin: TPluginComponent; GuiClass: TFormClass);
begin
  FPlugin:=Plugin;
  FGui:=GuiClass.Create(nil);
  FParent := nil;
  FGui.Left:=0;
  FGui.Top := 0;
  FGui.BorderStyle:=bsNone;
end;

destructor TPluginEditor.Destroy;
begin
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

function TPluginEditor.GetRect(rect: PPERect): boolean;
begin
  FRect.Left := 0;
  FRect.Top := 0;
  FRect.Right := FGui.Width;
  FRect.Bottom := FGui.Height;
  rect^:=@FRect;
  Result:=True;
end;

function TPluginEditor.Open(ptr: Pointer): boolean;
begin
  FParent := ptr;
  FGui.ParentWindow := ToIntPtr(FParent);
  FGui.Show;
  Result := True;
end;

procedure TPluginEditor.Close;
begin
  FGui.Hide;
  FParent := nil;
end;

function TPluginEditor.IsOpen: boolean;
begin
  Result:=FParent<>nil;
end;

procedure TPluginEditor.Idle;
begin
end;

{$ifdef VST_2_1_EXTENSIONS}
function TPluginEditor.OnKeyDown(var keyCode: TVstKeyCode): boolean;
begin
  Result:=False;
end;

function TPluginEditor.OnKeyUp(var keyCode: TVstKeyCode): boolean;
begin
  Result:=False;
end;

function TPluginEditor.OnWheel(distance: single): boolean;
begin
  Result:=False;
end;

function TPluginEditor.SetKnobMode(val: Int32): boolean;
begin
  Result:=False;
end;
{$endif}

{ TPluginComponent }

constructor TPluginComponent.Create(VstHost: TVstHostCallback);
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
  FEffect.UniqueID := MakeUniqueID('N', 'o', 'E', 'f');
  FEffect.Version := 1;
  FEffect.ProcessReplacing := @Process32Cb;
{$ifdef VST_2_4_EXTENSIONS}
  SetEffectFlag(effFlagsCanReplacing); // mandatory in VST 2.4!
  FEffect.ProcessDoubleReplacing := @Process64Cb;
{$endif}
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
  inherited Destroy;
end;

procedure TPluginComponent.SetProgram(index: int32);
var
  i: Integer;
begin
  if index<FNumPrograms then
  begin
    for i:=0 to FNumParams-1 do
      FParamInfos[i].Value:=FPresetInfos[index].Params[i];
    FCurProgram := index;
  end;
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
  if index<FNumParams then
  with FParamInfos[index] do
  case DisplayMode of
    pdmFloat:Result:=Format('%.7f',[Value]);
    pdmdB:Result:=Format('%.7f',[Log10(Value)*20]);
    pdmInteger:Result:=IntToStr(Trunc(Value));
    pdmMs:;
    pdmHz:;
    else Result:='';
  end;
end;

function TPluginComponent.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr;
{$ifdef VST_2_1_EXTENSIONS}
var
  KeyCode: TVstKeyCode;
{$endif}
begin
  Result:=0;
  case opcode of
    effOpen:;
    effClose:;
    effSetProgram: SetProgram(Value);
    effGetProgram: Result:=FCurProgram;
    effSetProgramName: FPresetInfos[FCurProgram].Name := StrPas(ptr);
    effGetProgramName: VstStrncpy(ptr,PAnsiChar(FPresetInfos[FCurProgram].Name),23);
    effGetParamLabel: VstStrncpy(ptr,PAnsiChar(FParamInfos[index].Lbl),7);
    effGetParamDisplay: VstStrncpy(ptr,PAnsiChar(GetParamDisplay(index)),7);
    effGetParamName: VstStrncpy(ptr,PAnsiChar(FParamInfos[index].Name),15);
{$ifndef VST_FORCE_DEPRECATED}
    effGetVu:;
{$endif}
    effSetSampleRate: FSampleRate := opt;
    effSetBlockSize: FBlockSize := Value;
    effMainsChanged:;
    effEditGetRect: Result:=IntPtr(FEditor.GetRect(ptr));
    effEditOpen: Result:=IntPtr(FEditor.Open(ptr));
    effEditClose: FEditor.Close;
    effEditIdle: FEditor.Idle;
    effIdentify: Result:=FEffect.UniqueID; // deprecated
    effGetChunk: ;
    effSetChunk: ;

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
    effVendorSpecific: ;
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
                                 ptr:=FParamInfos[index].Properties;
                                 Result:=1;
                               end else Result:=0;
    effGetVstVersion: Result:=kVstVersion;
{$ifdef VST_2_1_EXTENSIONS}
    effEditKeyDown: begin
                      KeyCode.Character := index;
                      KeyCode.modifier := TVstModifierKeys(Value);
                      KeyCode.virt := TVstVirtualKey(Trunc(opt));
                      Result := IntPtr(FEditor.OnKeyDown(KeyCode));
                    end;
    effEditKeyUp: begin
                    KeyCode.Character := index;
                    KeyCode.modifier := TVstModifierKeys(Value);
                    KeyCode.virt := TVstVirtualKey(Trunc(opt));
                    Result := IntPtr(FEditor.OnKeyUp(KeyCode));
                  end;
    effSetEditKnobMode: Result:=IntPtr(FEditor.SetKnobMode(Value));
    effGetMidiProgramName: ;
    effGetCurrentMidiProgram: Result:=-1;
    effGetMidiProgramCategory: ;
    effHasMidiProgramsChanged: ;
    effGetMidiKeyName: ;
    effBeginSetProgram: ;
    effEndSetProgram: ;
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement: ;
    effShellGetNextPlugin: ; // Only called if is kPlugCategShell
    effStartProcess: ;
    effStopProcess: ;
    effSetTotalSampleToProcess: Result:=Value;
    effSetPanLaw: ;
    effBeginLoadBank: ;
    effBeginLoadProgram: ;
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision: ;
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
    if FParamInfos[index].CanBeAutomated then
      FHost(AEffect,amAutomate,index,0,nil,value); // Can be called only once !
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

procedure TPluginComponent.SetNumberInOut(InNum, OutNum: Int32);
begin
  FEffect.NumInputs := InNum;
  FEffect.NumOutputs := OutNum;
end;

procedure TPluginComponent.SetEditor(Editor: TPluginEditor);
begin
  if Assigned(Editor) then begin
    FEditor:=Editor;
    Include(FEffect.Flags,effFlagsHasEditor);
  end;
end;

procedure TPluginComponent.SetUniqueID(A, B, C, D: AnsiChar);
begin
  FEffect.UniqueID := MakeUniqueID(A,B,C,D);
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

