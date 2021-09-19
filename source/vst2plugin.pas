{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugin
// Description : Basic plugin classes
// Created by  : PeaZomboss, 2021/07

// Basically have features for plugin except gui
// Use this unit can develop a non-gui plugin
// Or use vst2plugui unit to add gui support
-------------------------------------------------------------------------------}

unit vst2plugin;

{$I vcompiler.inc}
// In fpc delphi mode, FPC default closed
// In delphi, supported since delphi 2009
{$PointerMath on}

interface

uses
  vst2intf,vst2plugbas;

type
  { TVPlugin }

  TVPlugin = class
  private
    FPluginBase:TVPluginBase;
    FEditorBase:TVEditorBase;
    FBase:IVPlugBase;
    FParam:IVParam;
    FPreset:IVPreset;
    FMidi:IVMidi;
    FEditor:IVEditor;
  protected
    function Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;virtual;
    function GetParameter(index:Integer):Single;virtual;
    procedure SetParameter(index:Integer;value:Single);virtual;
    procedure Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
{$ifdef VST_2_4_EXTENSIONS}
    procedure ProcessRep64(const inputs,outputs:TBuffer64;SampleFrames:Int32);virtual;
{$endif}
    procedure SetEditor(AEditor:IVEditor);
    //procedure ProcessMidiEvent(const Event:TVstMidiEvent);virtual;
  public
    constructor Create(AHost:THostCallback);virtual;
    destructor Destroy;override;
    function GetAEffect:PAEffect;
    property Base:IVPlugBase read FBase;
    property Param:IVParam read FParam;
    property Preset:IVPreset read FPreset;
    property MIDI:IVMidi read FMidi;
    property Editor:IVEditor read FEditor;
  end;

  TVPluginClass = class of TVPlugin;

implementation

uses
  sysutils;


{$if defined(debug) and not defined(FPC)}
function StrPas(P:Pointer):AnsiString;
begin
  Result:=PAnsiChar(P);
end;
{$endif}


// Callback methods in TAEffect

function DispatcherCb(e:PAEffect;opcode,index:Int32;value:IntPtr;ptr:Pointer;opt:single):IntPtr;cdecl;

{$ifdef debug}
  procedure logdbg;
  begin
    if (opcode>=0) and (opcode<kVstAEOpcodeMax) then
    case TAEOpcodes(opcode) of
      effOpen: dbgln('Host open the plugin');
      effClose: dbgln('Host close the plugin');
      effSetProgram: dbgln('Host set preset: %d',[value]);
      effGetProgram: dbgln('Host get preset');
      effSetProgramName: dbgln('Host set preset name: %s',[strpas(ptr)]);
      effGetProgramName: dbgln('Host get preset name');
      effGetParamLabel: dbgln('Host get param label, index: %d',[index]);
      effGetParamDisplay: dbgln('Host get param display, index: %d',[index]);
      effGetParamName: dbgln('Host get param name, index: %d',[index]);
      effGetVu: ;
      effSetSampleRate: dbgln('Host set sample rate: %.3f',[opt]);
      effSetBlockSize: dbgln('Host set block size: %d',[value]);
      effMainsChanged: if value=0 then dbgln('Host turn off the plugin')
                       else dbgln('Host turn on the plugin');
      effEditGetRect: dbgln('Host get editor rect');
      effEditOpen: dbgln('Host open editor, parent: %p',[ptr]);
      effEditClose: dbgln('Host close editor');
      effEditDraw: ;
      effEditMouse: ;
      effEditKey: ;
      effEditIdle: ; // lots
      effEditTop: ;
      effEditSleep: ;
      effIdentify: ;
      effGetChunk: dbgln('Host get chunk');
      effSetChunk: dbgln('Host set chunk, size: %d',[value]);
      effProcessEvents: ; // lots
      effCanBeAutomated: dbgln('Host get can be automated,index: %d',[index]);
      effString2Parameter: dbgln('Host string 2 parameter, index: %d, str: %s',[index,strpas(ptr)]);
      effGetNumProgramCategories: ;
      effGetProgramNameIndexed: dbgln('Host get preset name, index: %d',[index]);
      effCopyProgram: ;
      effConnectInput: ;
      effConnectOutput: ;
      effGetInputProperties: dbgln('Host get in properties, index: %d',[index]);
      effGetOutputProperties: dbgln('Host get out properties, index: %d',[index]);
      effGetPlugCategory: dbgln('Host get plug category');
      effGetCurrentPosition: ;
      effGetDestinationBuffer: ;
      effOfflineNotify: ;
      effOfflinePrepare: ;
      effOfflineRun: ;
      effProcessVarIO: ;
      effSetSpeakerArrangement: dbgln('Host set speaker arrangement');
      effSetBlockSizeAndSampleRate: dbgln('Host set blocksize %d and samplerate %.3f',[value,opt]);
      effSetBypass: ;
      effGetEffectName: dbgln('Host get effect name');
      effGetErrorText: ;
      effGetVendorString: dbgln('Host get vendor string');
      effGetProductString: dbgln('Host get product string');
      effGetVendorVersion: dbgln('Host get vendor version');
      effVendorSpecific: dbgln('Host vender specific, %x, %x, %p, %.3f',[index,value,ptr,opt]);
      effCanDo: dbgln('Host ask can do: %s',[StrPas(ptr)]);
      effGetTailSize: dbgln('Host get tail size');
      effIdle: ;
      effGetIcon: ;
      effSetViewPosition: ;
      effGetParameterProperties: dbgln('Host get parameter properties, index: %d',[index]);
      effKeysRequired: ;
      effGetVstVersion: dbgln('Host get vst version');
{$ifdef VST_2_1_EXTENSIONS}
      effEditKeyDown: dbgln('Host call key down, %d, %d, %.3f',[index,value,opt]);
      effEditKeyUp: dbgln('Host call key up, %d, %d, %.3f',[index,value,opt]);
      effSetEditKnobMode: dbgln('Host set edit knob mode: %d',[value]);
      effGetMidiProgramName: dbgln('Host get midi program name, channel: %d',[index]);
      effGetCurrentMidiProgram: dbgln('Host get current midi program, channel: %d',[index]);
      effGetMidiProgramCategory: dbgln('Host get midi program category, channel: %d',[index]);
      effHasMidiProgramsChanged: dbgln('Host has midi programs changed, channel: %d',[index]);
      effGetMidiKeyName: dbgln('Host get midi key name, channel: %d',[index]);
      effBeginSetProgram: dbgln('Host begin set program');
      effEndSetProgram: dbgln('Host end set program');
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
      effGetSpeakerArrangement: dbgln('Host get speaker arrangement');
      effShellGetNextPlugin: dbgln('Host get next plugin');
      effStartProcess: dbgln('Host start process');
      effStopProcess: dbgln('Host stop process');
      effSetTotalSampleToProcess: dbgln('Host set total sample to process, num: %d',[value]);
      effSetPanLaw: dbgln('Host set pan law: %d, gain: %.3f',[value,opt]);
      effBeginLoadBank: dbgln('Host begin load bank');
      effBeginLoadProgram: dbgln('Host begin load program');
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
      effSetProcessPrecision: dbgln('Host set process precision: %d',[value]);
      effGetNumMidiInputChannels: dbgln('Host get midi in channels');
      effGetNumMidiOutputChannels: dbgln('Host get midi out channels');
{$endif}
    end;
  end;
{$endif}

var
  v:TVPlugin;
begin
  {$ifdef debug}logdbg;{$endif}
  if (opcode>=0) and (opcode<kVstAEOpcodeMax) then
  begin
    v:=TVPlugin(e^.Obj);
    if opcode <> ord(effClose) then
      Result:=v.Dispatcher(TAEOpcodes(opcode),index,value,ptr,opt)
    else begin
      v.Dispatcher(TAEOpcodes(opcode),index,value,ptr,opt);
      v.Free;
      Result:=1;
      {$ifdef debug}dbgln('Plugin closed');{$endif}
    end;
  end else begin
    Result:=0;
{$ifdef debug}
    dbgln('Unknown eff opcode: %d, index: %d, value: %d, ptr: %p or str: %s, opt: %.3f',
      [opcode,index,value,ptr,strpas(ptr),opt]);
{$endif}
  end;
end;

function GetParameterCb(e:PAEffect;index:Int32):single;cdecl;
begin
  {$ifdef debug}dbgln('GetParameterCb index: %d',[index]);{$endif}
  Result:=TVPlugin(e^.Obj).GetParameter(index);
end;

procedure SetParameterCb(e:PAEffect;index:Int32;value:single);cdecl;
begin
  {$ifdef debug}dbgln('SetParameterCb index: %d, value: %.3f',[index,value]);{$endif}
  TVPlugin(e^.Obj).SetParameter(index,value);
end;

procedure ProcessCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.Obj).Process(inputs,outputs,sampleframes);
end;

procedure ProcessRepCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.Obj).ProcessRep(inputs,outputs,sampleframes);
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure ProcessRep64Cb(e:PAEffect;inputs,outputs:PPDouble;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.Obj).ProcessRep64(inputs,outputs,sampleframes);
end;
{$endif}

{ TVPlugin }

constructor TVPlugin.Create(AHost:THostCallback);
begin
  FPluginBase:=TVPluginBase.Create(AHost,self);
  with FPluginBase.GetEffect^ do
  begin
    Dispatcher:=@DispatcherCb;
    GetParameter:=@GetParameterCb;
    SetParameter:=@SetParameterCb;
    Process:=@ProcessCb;
    ProcessReplacing:=@ProcessRepCb;
{$ifdef VST_2_4_EXTENSIONS}
    ProcessDoubleReplacing:=@ProcessRep64Cb;
    Include(Flags,effFlagsCanReplacing);
{$endif}
  end;
  FPluginBase.GetInterface(iidIVPlugBase,FBase);
  FPluginBase.GetInterface(iidIVParam,FParam);
  FPluginBase.GetInterface(iidIVPreset,FPreset);
  FPluginBase.GetInterface(iidIVMidi,FMidi);
end;

destructor TVPlugin.Destroy;
begin
  inherited Destroy;
end;

function TVPlugin.GetAEffect:PAEffect;
begin
  Result:=FPluginBase.GetEffect;
end;

procedure TVPlugin.SetEditor(AEditor:IVEditor);
begin
  if Assigned(AEditor) and not Assigned(FEditor) then
  begin
    FEditor:=AEditor;
    FEditorBase:=FEditor as TVEditorBase;
  end;
end;

function TVPlugin.Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=0;
  case opcode of
    effOpen: ;
    effClose: ;
    effSetProgram: FPluginBase.SetPreset(value);
    effGetProgram: Result:=FPluginBase.GetPreset;
    effSetProgramName: FPluginBase.SetPresetName(ptr);
    effGetProgramName: VstStrncpy(ptr,FPluginBase.GetPresetName,23);
    effGetParamLabel: VstStrncpy(ptr,FPluginBase.GetParamLabel(index),7);
    effGetParamDisplay: VstStrncpy(ptr,FPluginBase.GetParamDisplay(index),15);
    effGetParamName: VstStrncpy(ptr,FPluginBase.GetParamName(index),15);
    effGetVu: ;
    effSetSampleRate: FPluginBase.SetSampleRate(opt);
    effSetBlockSize: FPluginBase.SetBlockSize(value);
    effMainsChanged: ;
    effEditOpen: if Assigned(FEditorBase) then FEditorBase.Open(ptr);
    effEditClose: if Assigned(FEditorBase) then FEditorBase.Close;
    effEditIdle: if Assigned(FEditorBase) then FEditorBase.Idle;
    effEditGetRect: if Assigned(FEditorBase) then FEditorBase.GetRect(ptr);
    effIdentify: ;
    effGetChunk: Result:=FPluginBase.GetChunk(ptr);
    effSetChunk: FPluginBase.SetChunk(ptr,value);
    effProcessEvents: FPluginBase.ProcessEvents(ptr);
    effCanBeAutomated: Result:=FPluginBase.CanBeAutomated(index);
    effString2Parameter: ;
    effGetNumProgramCategories: ;
    effGetProgramNameIndexed: Result:=FPluginBase.GetPresetNameIndexed(index,ptr);
    effCopyProgram: ;
    effConnectInput: ;
    effConnectOutput: ;
    effGetInputProperties: ;
    effGetOutputProperties: ;
    effGetPlugCategory: Result:=FPluginBase.GetPlugCategory;
    effGetCurrentPosition: ;
    effGetDestinationBuffer: ;
    effOfflineNotify: ;
    effOfflinePrepare: ;
    effOfflineRun: ;
    effProcessVarIO: ;
    effSetSpeakerArrangement: ;
    effSetBlockSizeAndSampleRate: with FPluginBase do
                                  begin
                                    SetBlockSize(value);
                                    SetSampleRate(opt);
                                  end;
    effSetBypass: ;
    effGetEffectName: VstStrncpy(ptr,FPluginBase.PlugName,31);
    effGetErrorText: ;
    effGetVendorString: VstStrncpy(ptr,FPluginBase.VendorName,63);
    effGetProductString: VstStrncpy(ptr,FPluginBase.ProductName,63);
    effGetVendorVersion: Result:=FPluginBase.VendorVersion;
    effVendorSpecific: Result:=FPluginBase.VendorSpecific(index,value,ptr,opt);
    effCanDo: Result:=FPluginBase.CanDo(ptr);
    effGetTailSize: ;
    effSetViewPosition: ;
    effGetParameterProperties: Result:=FPluginBase.GetParameterProperties(index,ptr);
    effKeysRequired: ;
    effGetVstVersion: Result:=kVstVersion;
{$ifdef VST_2_1_EXTENSIONS}
    effGetMidiProgramName: ;
    effGetCurrentMidiProgram: ;
    effGetMidiProgramCategory: ;
    effHasMidiProgramsChanged: ;
    effGetMidiKeyName: ;
    effBeginSetProgram: ;
    effEndSetProgram: ;
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement: ;
    effShellGetNextPlugin: ;
    effStartProcess: ;
    effStopProcess: ;
    effSetTotalSampleToProcess: ;
    effSetPanLaw: ;
    effBeginLoadBank: ;
    effBeginLoadProgram: ;
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision: FPluginBase.SetProcessPrecision(Value);
    effGetNumMidiInputChannels: ;
    effGetNumMidiOutputChannels: ;
{$endif}
    else ;
  end;
end;

function TVPlugin.GetParameter(index:Integer):Single;
begin
  Result:=FParam.GetParameter(index);
end;

procedure TVPlugin.SetParameter(index:Integer;value:Single);
begin
  FParam.SetParameter(index,value);
end;

procedure TVPlugin.Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:integer;
begin
  if FPluginBase.DefaultIO then
    for i:=0 to SampleFrames-1 do
    begin
      outputs[0,i]:=inputs[0,i];
      outputs[1,i]:=inputs[1,i];
    end;
end;

procedure TVPlugin.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:integer;
begin
  if FPluginBase.DefaultIO then
    for i:=0 to SampleFrames-1 do
    begin
      outputs[0,i]:=inputs[0,i];
      outputs[1,i]:=inputs[1,i];
    end;
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVPlugin.ProcessRep64(const inputs,outputs:TBuffer64;SampleFrames:Int32);
var
  i:integer;
begin
  if FPluginBase.DefaultIO then
    for i:=0 to SampleFrames-1 do
    begin
      outputs[0,i]:=inputs[0,i];
      outputs[1,i]:=inputs[1,i];
    end;
end;
{$endif}

end.

