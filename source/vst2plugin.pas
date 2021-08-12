{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugin
// Description : Basic plugin classes
// Created by  : PeaZomboss, 2021/07

// Basically have features for plugin except gui
// Use this unit when developing a non-gui plugin
-------------------------------------------------------------------------------}

unit vst2plugin;

{$I vcompiler.inc}

interface

uses
  vst2intf,vst2plugbas;

type
  { TVPlugin }

  TVPlugin = class(TInterfacedObject,IVPlugBase,IVParam,IVPreset)
  private
    FPluginBase:TVPluginBase;
    FBase:IVPlugBase;
    FParam:IVParam;
    FPreset:IVPreset;
  protected
    procedure Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
{$ifdef VST_2_4_EXTENSIONS}
    procedure ProcessRep64(const inputs,outputs:TBuffer64;SampleFrames:Int32);virtual;
{$endif}
    function Dispatcher(opcode:TAEOpcodes;index:Int32;const value:IntPtr;const ptr:Pointer;opt:Single):IntPtr;virtual;
  public
    constructor Create(AHost:THostCallback);//virtual;
    destructor destroy;override;
    property Base:IVPlugBase read FBase implements IVPlugBase;
    property Param:IVParam read FParam implements IVParam;
    property Preset:IVPreset read FPreset implements IVPreset;
  end;

  //TVPluginClass = class of TVPlugin;

var
  VDispatcher:TAEDispatcherCb;
  VGetParameter:TAEGetParamCb;
  VSetParameter:TAESetParamCb;
  VProcess:TAEProcess32Cb;
  VProcessRep:TAEProcess32Cb;
  VProcessRep64:TAEProcess64Cb;

//function DoVstMain(AHost:THostCallback;PluginClass:TVPluginClass):PAEffect;

implementation

uses
  sysutils;

(*
// Require virtual constructor
function DoVstMain(AHost:THostCallback;PluginClass:TVPluginClass):PAEffect;
begin
  {$ifdef debug}dbgln('Plugin init start');{$endif}
  try
    Result:=PluginClass.Create(AHost).Base.Effect;
  except
    on E:Exception do
    {$ifdef debug}dbgln('%s: %s',[E.ClassName,E.Message]);{$endif}
  end;
  {$ifdef debug}dbgln('Plugin init done');{$endif}
end;
*)

// Callback methods in TAEffect

function DispatcherCb(e:PAEffect;opcode,index:Int32;Value:IntPtr;ptr:Pointer;opt:single):IntPtr;cdecl;

{$ifdef debug}
  procedure logdbg;
  begin
    if (opcode>=0) and (opcode<kVstEffOpcodeNum) then
    case TAEOpcodes(opcode) of
      effOpen: ;
      effClose: ;
      effSetProgram: dbgln('Host set preset: %d',[value]);
      effGetProgram: ;
      effSetProgramName: dbgln('Host set preset name: %s',[strpas(ptr)]);
      effGetProgramName: ;
      effGetParamLabel: ;
      effGetParamDisplay: ;
      effGetParamName: ;
      effGetVu: ;
      effSetSampleRate: dbgln('Host set sample rate: %.5f',[opt]);
      effSetBlockSize: dbgln('Host set block size: %d',[value]);
      effMainsChanged: if value=0 then dbgln('Host turn off the plugin')
                       else dbgln('Host turn on the plugin');
      effEditGetRect: ;
      effEditOpen: ;
      effEditClose: ;
      effEditDraw: ;
      effEditMouse: ;
      effEditKey: ;
      effEditIdle: ;
      effEditTop: ;
      effEditSleep: ;
      effIdentify: ;
      effGetChunk: dbgln('Host get chunk');
      effSetChunk: dbgln('Host set chunk, size: %d',[value]);
      effProcessEvents: ;
      effCanBeAutomated: ;
      effString2Parameter: ;
      effGetNumProgramCategories: ;
      effGetProgramNameIndexed: ;
      effCopyProgram: ;
      effConnectInput: ;
      effConnectOutput: ;
      effGetInputProperties: ;
      effGetOutputProperties: ;
      effGetPlugCategory: ;
      effGetCurrentPosition: ;
      effGetDestinationBuffer: ;
      effOfflineNotify: ;
      effOfflinePrepare: ;
      effOfflineRun: ;
      effProcessVarIO: ;
      effSetSpeakerArrangement: ;
      effSetBlockSizeAndSampleRate: ;
      effSetBypass: ;
      effGetEffectName: ;
      effGetErrorText: ;
      effGetVendorString: ;
      effGetProductString: ;
      effGetVendorVersion: ;
      effVendorSpecific: ;
      effCanDo: dbgln('Host ask can do: %s',[StrPas(ptr)]);
      effGetTailSize: ;
      effIdle: ;
      effGetIcon: ;
      effSetViewPosition: ;
      effGetParameterProperties: ;
      effKeysRequired: ;
      effGetVstVersion: ;
      effEditKeyDown: ;
      effEditKeyUp: ;
      effSetEditKnobMode: dbgln('Host set edit knob mode: %d',[value]);
      effGetMidiProgramName: ;
      effGetCurrentMidiProgram: ;
      effGetMidiProgramCategory: ;
      effHasMidiProgramsChanged: ;
      effGetMidiKeyName: ;
      effBeginSetProgram: ;
      effEndSetProgram: ;
      effGetSpeakerArrangement: ;
      effShellGetNextPlugin: ;
      effStartProcess: ;
      effStopProcess: ;
      effSetTotalSampleToProcess: ;
      effSetPanLaw: dbgln('Host set pan law: %d, gain: %.5f',[value,opt]);
      effBeginLoadBank: ;
      effBeginLoadProgram: ;
      effSetProcessPrecision: dbgln('Host set process precision: %d',[value]);
      effGetNumMidiInputChannels: ;
      effGetNumMidiOutputChannels: ;
    end;
  end;
{$endif}

var
  v:TVPlugin;
begin
  {$ifdef debug}logdbg;{$endif}
  if (opcode>=0) and (opcode<kVstEffOpcodeNum) then
  begin
    v:=TVPlugin(e^.pObject);
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
    dbgln('Unknown eff opcode: %d, index: %d, value: %d, ptr: %p or str: %s, opt: %.5f',
      [opcode,index,value,ptr,strpas(ptr),opt]);
{$endif}
  end;
end;

function GetParameterCb(e:PAEffect;index:Int32):single;cdecl;
begin
  {$ifdef debug}dbgln('GetParameterCb index: %d',[index]);{$endif}
  Result:=TVPlugin(e^.pObject).Param[index];
end;

procedure SetParameterCb(e:PAEffect;index:Int32;value:single);cdecl;
begin
  {$ifdef debug}dbgln('SetParameterCb index: %d, value: %.5f',[index,value]);{$endif}
  TVPlugin(e^.pObject).Param[index]:=value;
end;

procedure ProcessCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.pObject).Process(inputs,outputs,sampleframes);
end;

procedure ProcessRepCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.pObject).ProcessRep(inputs,outputs,sampleframes);
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure ProcessRep64Cb(e:PAEffect;inputs,outputs:PPDouble;sampleFrames:Int32);cdecl;
begin
  TVPlugin(e^.pObject).ProcessRep64(inputs,outputs,sampleframes);
end;
{$endif}

{ TVPlugin }

constructor TVPlugin.Create(AHost:THostCallback);
begin
  FPluginBase:=TVPluginBase.Create(AHost,self);
  FPluginBase.GetInterface(iidIVBase,FBase);
  FPluginBase.GetInterface(iidIVParam,FParam);
  FPluginBase.GetInterface(iidIVPreset,FPreset);
  //{$ifdef debug}dbgln('TVPlugin create');{$endif}
end;

destructor TVPlugin.destroy;
begin
  //{$ifdef debug}dbgln('TVPlugin destroy');{$endif}
  inherited destroy;
end;

function TVPlugin.Dispatcher(opcode:TAEOpcodes;index:Int32;const value:IntPtr;const ptr:Pointer;opt:Single):IntPtr;
begin
  //{$ifdef debug}
  //dbgln('opcode: %s, index: %d, value: %d, ptr: %p, opt: %.5f',
  //  [VstAEOpcode2Str(opcode),index,value,ptr,opt]);
  //{$endif}
  Result:=0;
  case opcode of
    effOpen: ;
    effClose: ;
    effSetProgram: FPluginBase.SetPreset(value);
    effGetProgram: Result:=FPluginBase.GetPreset;
    effSetProgramName: FPluginBase.SetPresetName(StrPas(ptr));
    effGetProgramName: VstStrncpy(ptr,FPluginBase.GetPresetName,23);
    effGetParamLabel: VstStrncpy(ptr,FPluginBase.GetParamLabel(index),7);
    effGetParamDisplay: VstStrncpy(ptr,FPluginBase.GetParamDisplay(index),15);
    effGetParamName: VstStrncpy(ptr,FPluginBase.GetParamName(index),15);
    effGetVu: ;
    effSetSampleRate: FPluginBase.SetSampleRate(opt);
    effSetBlockSize: FPluginBase.SetBlockSize(value);
    effMainsChanged: ;
    effIdentify: ;
    effGetChunk: Result:=FPluginBase.GetChunk(ptr);
    effSetChunk: FPluginBase.SetChunk(ptr,value);
    effProcessEvents: ;
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
    effCanDo: ;
    effGetTailSize: ;
    effSetViewPosition: ;
    effGetParameterProperties: Result:=FPluginBase.GetParameterProperties(index,ptr);
    effKeysRequired: ;
    effGetVstVersion: Result:=kVstVersion;
    effGetMidiProgramName: ;
    effGetCurrentMidiProgram: ;
    effGetMidiProgramCategory: ;
    effHasMidiProgramsChanged: ;
    effGetMidiKeyName: ;
    effBeginSetProgram: ;
    effEndSetProgram: ;
    effGetSpeakerArrangement: ;
    effShellGetNextPlugin: ;
    effStartProcess: ;
    effStopProcess: ;
    effSetTotalSampleToProcess: ;
    effSetPanLaw: ;
    effBeginLoadBank: ;
    effBeginLoadProgram: ;
    effSetProcessPrecision: ;
    effGetNumMidiInputChannels: ;
    effGetNumMidiOutputChannels: ;
    else ;
  end;
end;

procedure TVPlugin.Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:integer;
begin
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
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=inputs[0,i];
    outputs[1,i]:=inputs[1,i];
  end;
end;
{$endif}

initialization

//{$ifdef debug}dbgln('Init vst2plugin unit');{$endif}
VDispatcher:=@DispatcherCb;
VGetParameter:=@GetParameterCb;
VSetParameter:=@SetParameterCb;
VProcess:=@ProcessCb;
VProcessRep:=@ProcessRepCb;
VProcessRep64:=@ProcessRep64Cb;

end.

