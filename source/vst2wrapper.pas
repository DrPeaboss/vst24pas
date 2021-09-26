{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2wrapper
// Description : Simple wrapper for vst2 plugin
// Created by  : PeaZomboss, 2021/09

// A simple wrapper which you need do anything yourself
// This unit is only for VST 2.4
-------------------------------------------------------------------------------}

unit vst2wrapper;

{$I vst2def.inc}

interface

uses
  vst2intf;

type
  // Classes in this unit
  TVst2Wrapper = class;
  TVst2Editor = class;

  { TVst2Wrapper }

  TVst2Wrapper = class
  private
    FHost:THostCallback;
    FSampleRate:Single;
    FBlockSize:Int32;
    FNumPresets:Int32;
    FNumParams:Int32;
    FCurPreset:Int32;
    FEffect:TAEffect;
    FEditor:TVst2Editor;
  private // Inside methods
    procedure SetEditor(AEditor:TVst2Editor);
    function CallHost(opcode:TAMOpcodes;index:Int32;value:IntPtr=0;ptr:Pointer=nil;opt:Single=0):IntPtr;overload;
    function CallHost(opcode:TAMOpcodes):IntPtr;overload;
    procedure ProcessEvents(events:PVstEvents);
    procedure EditGetRect(rect:PPERect);
    procedure EditOpen(parent:Pointer);
    procedure EditClose;
    procedure EditIdle;
{$ifdef VST_2_1_EXTENSIONS}
    function EditKeyDown(charactor:Int32;virt,modifier:Byte):Int32;
    function EditKeyUp(charactor:Int32;virt,modifier:Byte):Int32;
    procedure SetEditKnobMode(mode:Int32);
{$endif}
  protected  // Methods which should be overrode
    function Dispatcher(opcode:TAEffectOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;virtual;
    function GetParameter(index:Int32):Single;virtual;
    procedure SetParameter(index:Int32;value:Single);virtual;
    procedure Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);virtual;
{$ifdef VST_2_4_EXTENSIONS}
    procedure ProcessRep64(const inputs,outputs:TBuffer64;SampleFrames:Int32);virtual;
{$endif}
    // Called before plugin open
    procedure Open;virtual;
    // Called before plugin close
    procedure Close;virtual;
    // Called before plugin suspend
    procedure Suspend;virtual;
    // Called before plugin resume
    procedure Resume;virtual;
    // Called when setting sample rate
    procedure SetSampleRate(SampleRate:Single);virtual;
    // Called when setting block size
    procedure SetBlockSize(BlockSize:Int32);virtual;
    // Called when setting preset
    procedure SetPreset(preset:Int32);virtual;
    // Get current preset
    function GetPreset:Int32;virtual;
    // Set current preset name
    procedure SetPresetName(name:PAnsiChar);virtual;
    // Get current preset name
    procedure GetPresetName(name:PAnsiChar);virtual;
    // Get preset name by index
    function GetPresetNameIndexed(index:Int32;text:PAnsiChar):Boolean;virtual;
    // Get parameter label (dB, % ..) by index
    procedure GetParamLabel(index:Int32;text:PAnsiChar);virtual;
    // Get parameter display by index
    procedure GetParamDisplay(index:Int32;text:PAnsiChar);virtual;
    // Get parameter name (Gain, Pan ..) by index
    procedure GetParamName(index:Int32;text:PAnsiChar);virtual;
    // Get parameters array
    function GetChunk(Data:PPointer;IsPreset:Boolean=False):Int32;virtual;
    // Set parameters from chunk (array) provided
    function SetChunk(Data:Pointer;ByteSize:Int32;IsPreset:Boolean=False):Int32;virtual;
    // Called when midi event in
    procedure ProcessMidiEvent(const event:TVstMidiEvent);virtual;
    // Called when midi sysex event in
    procedure ProcessMidiSysexEvent(const event:TVstMidiSysexEvent);virtual;
    // Query a parameter can be automated
    function ParamCanBeAutomated(index:Int32):Boolean;virtual;
    // Convert a string to parameter
    function String2Parameter(index:Int32;str:PAnsiChar):Boolean;virtual;
    // Set bypass
    procedure SetBypass(OnOff:Boolean);virtual;
    // Get plugin category, see TVstPlugCategory
    function GetPlugCategory:TVstPlugCategory;virtual;
    // Get plugin name
    procedure GetEffectName(text:PAnsiChar);virtual;
    // Get vendor name
    procedure GetVendorString(text:PAnsiChar);virtual;
    // Get product name
    procedure GetProductString(text:PAnsiChar);virtual;
    // Get vendor version
    function GetVendorVersion:Int32;virtual;
    // Defined by vendor
    function VendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;virtual;
    // Query plugin can do, see TPcdStrings
    function CanDo(text:PAnsiChar):Int32;virtual;
    // Get tail size
    function GetTailSize:Int32;virtual;
    // See TVstPinProperties
    function GetInputProperties(index:Int32;properties:PVstPinProperties):Int32;virtual;
    // See TVstPinProperties
    function GetOutputProperties(index:Int32;properties:PVstPinProperties):Int32;virtual;
    // See TVstParameterProperties
    function GetParamProperties(index:Int32;properties:PVstParameterProperties):Int32;virtual;
    // See TMidiProgramName
    function GetMidiProgramName(index:Int32;Midipn:PMidiProgramName):Int32;virtual;
    // See TMidiProgramName
    function GetCurrentMidiProgram(index:Int32;Midipn:PMidiProgramName):Int32;virtual;
    // See TMidiProgramCategory
    function GetMidiProgramCategory(index:Int32;Midipc:PMidiProgramCategory):Int32;virtual;
    // Return true if the TMidiProgramNames, TMidiKeyNames or TMidiControllerNames had changed on this MIDI channel.
    function HasMidiProgramsChanged(index:Int32):Boolean;virtual;
    // See TMidiKeyName
    function GetMidiKeyName(index:Int32;Midikn:PMidiKeyName):Boolean;virtual;
    // Called before set preset
    procedure BeginSetProgram;virtual;
    // Called after set preset
    procedure EndSetProgram;virtual;
    // See TVstSpeakerArrangement
    procedure SetSpeakerArrangement(input,output:PVstSpeakerArrangement);virtual;
    // See TVstSpeakerArrangement
    procedure GetSpeakerArrangement(input,output:PVstSpeakerArrangement);virtual;
    // Called if the plugin's category is kPlugCategShell, return next plugin's unique ID
    function ShellGetNextPlugin(name:PAnsiChar):Int32;virtual;
    // Called before process
    procedure StartProcess;virtual;
    // Called after process
    procedure StopProcess;virtual;
    // Used for variable I/O processing (offline processing like timestreching)
    procedure ProcessVarIO(VarIO:PVstVariableIO);virtual;
    procedure OfflineNotify(AudioFiles:PVstAudioFile;count:IntPtr;start:Boolean);virtual;
    procedure OfflinePrepare(task:PVstOfflineTask;count:IntPtr);virtual;
    procedure OfflineRun(task:PVstOfflineTask;count:IntPtr);virtual;
    procedure SetTotalSampleToProcess(SampleNumber:IntPtr);virtual;
    // Set pan law, see TVstPanLawType
    procedure SetPanLaw(PanLaw:IntPtr;gain:Single);virtual;
    // Called before load bank
    function BeginLoadBank(patch:PVstPatchChunkInfo):Int32;virtual;
    // Called before load preset
    function BeginLoadPreset(patch:PVstPatchChunkInfo):Int32;virtual;
    // Set plugin's process precision, see TVstProcessPrecision
    procedure SetProcessPrecision(precision:IntPtr);virtual;
    // Get midi input channels number
    function GetNumMidiInputChannels:Int32;virtual;
    // Get midi output channels number
    function GetNumMidiOutputChannels:Int32;virtual;
  protected // Utility methods
    // Set plugin's unique ID
    procedure SetUniqueID(ID:Int32);
    // Set plugin's version
    procedure SetVersion(version:Int32);
    // Set plugin's input number
    procedure SetNumInput(num:Int32);
    // Set plugin's output number
    procedure SetNumOutput(num:Int32);
    // Set plugin's initial delay
    procedure SetInitialDelay(delay: Int32);
    // Set plugin can process in replacing mode
    procedure CanProcessReplacing;
    // Set plugin's preset can saved by chunk
    procedure PresetsAreChunk;
    // Set plugin as synth (default is effect)
    procedure SetAsSynth;
    // Plugin won't produce output signals while there is no input
    procedure SetNoTail;
{$ifdef VST_2_4_EXTENSIONS}
    // Set plugin can process in double replacing mode
    procedure CanDoubleReplacing;
{$endif}
  public
    constructor Create(AHost:THostCallback;NumPresets,NumParams:Int32);
    destructor Destroy;override;
    function GetEffect:PAEffect;
  public // Methods which call host
    // Set a parameter and tell host the parameter can be automated
    procedure SetParamAutomated(index:Int32;value:Single);
    // Get Host's vst version
    function GetHostVersion:Int32;
    // Get current unique ID
    function GetCurrentUniqueID:Int32;
    // Give host time to idle
    procedure HostIdle;
    // Get time info, see TVstTimeInfo, TVstTimeInfoFlags
    function GetTimeInfo(flags:TVstTimeInfoFlags):PVstTimeInfo;
    // Tell host NumInputs and/or NumOutputs and/or InitialDelay have changed
    function IOChanged:Boolean;
    // Let host process events
    procedure HostProcessEvents(events:PVstEvents);
    // Tell host to change plugin editor window size
    procedure SizeWindow(Width,Height:Integer);
    // Get and update sample rate
    function UpdateSampleRate:Single;
    // Get and update block size
    function UpdateBlockSize:Int32;
    // Get current process level, see TVstProcessLevels
    function GetCurrentProcessLevel:Int32;
    // Get automation state, see TVstAutomationStates
    function GetAutomationState:Int32;
    function OfflineStart(AudioFiles:PVstAudioFile;AudioFileNum,NewAudioFilesNum:Int32):Boolean;
    function OfflineRead(task:PVstOfflineTask;option:TVstOfflineOption;ReadSource:Boolean):Boolean;
    function OfflineWrite(task:PVstOfflineTask;option:TVstOfflineOption):Boolean;
    function OfflineGetCurrentPass:Int32;
    function OfflineGetCurrentMetaPass:Int32;
    // Get host's vendor name
    procedure GetHostVendorString(text:PAnsiChar);
    // Get host's product name
    procedure GetHostProductString(text:PAnsiChar);
    // Get host's vendor version
    function GetHostVendorVersion:Int32;
    // Defined by vendor
    function HostVendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
    // Get host can do, see THcdStrings
    function HostCanDo(text:PAnsiChar):Boolean;
    // Get host languate, see TVstHostLanguage
    function GetHostLanguage:Int32;
    // Get plugin library file's directory
    function GetDirectory:PAnsiChar;
    // Something has changed in plugin, request an update display like preset (MIDI too) and parameters list in host
    function UpdateDisplay:Boolean;
    // To be called before SetParamAutomated (on Mouse Down), host can do something
    function BeginEdit(index:Int32):Boolean;
    // To be called after SetParamAutomated (on Mouse Up)
    function EndEdit(index:Int32):Boolean;
    // Tell host open file selector, see TVstFileSelect
    function HostOpenFileSelector(FileSelect:PVstFileSelect):Boolean;
    // Tell host close file selector
    function HostCloseFileSelector(FileSelect:PVstFileSelect):Boolean;
  public // Properties
    property Parameter[index:Int32]:Single read GetParameter write SetParameter;
    property SampleRate:Single read FSampleRate;
    property BlockSize:Int32 read FBlockSize;
    property Editor:TVst2Editor read FEditor write SetEditor;
  end;

  { TVst2Editor }

  TVst2Editor = class
  private
    FPlugin:TVst2Wrapper;
    FParentWindow:Pointer;
  protected
    FERect:TERect;
  public
    constructor Create(APlugin:TVst2Wrapper);
    // Get editor's rect, return address of FERect
    procedure GetRect(rect:PPERect);virtual;
    // Open the editor
    procedure Open(parent:Pointer);virtual;
    // Close the editor
    procedure Close;virtual;
    // Make editor idle
    procedure Idle;virtual;
{$ifdef VST_2_1_EXTENSIONS}
    // Called when key down, return true if key used, see TVstVirtualKey, TVstModifierKeys
    function OnKeyDown(charactor:Int32;virt,modifier:Byte):Boolean;virtual;
    // Called when key up, return true if key used
    function OnKeyUp(charactor:Int32;virt,modifier:Byte):Boolean;virtual;
    // Set knob mode, 0: circular, 1: circular relativ, 2: linear
    procedure SetKnobMode(mode:Int32);virtual;
{$endif}
  public
    property Plugin:TVst2Wrapper read FPlugin;
  end;


implementation

{$Hints off}

function DispatcherCb(e:PAEffect;opcode,index:Int32;value:IntPtr;ptr:Pointer;opt:single):IntPtr;cdecl;
var
  v:TVst2Wrapper;
begin
  if (opcode>=0) and (opcode<kVstAEOpcodeMax) then
  begin
    v:=TVst2Wrapper(e^.Obj);
    if opcode <> ord(effClose) then
      Result:=v.Dispatcher(TAEOpcodes(opcode),index,value,ptr,opt)
    else begin
      v.Dispatcher(TAEOpcodes(opcode),index,value,ptr,opt);
      v.Free;
      Result:=1;
    end;
  end
  else
    Result:=0;
end;

function GetParameterCb(e:PAEffect;index:Int32):single;cdecl;
begin
  Result:=TVst2Wrapper(e^.Obj).GetParameter(index);
end;

procedure SetParameterCb(e:PAEffect;index:Int32;value:single);cdecl;
begin
  TVst2Wrapper(e^.Obj).SetParameter(index,value);
end;

procedure ProcessCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVst2Wrapper(e^.Obj).Process(inputs,outputs,sampleframes);
end;

procedure ProcessRepCb(e:PAEffect;inputs,outputs:PPSingle;sampleFrames:Int32);cdecl;
begin
  TVst2Wrapper(e^.Obj).ProcessRep(inputs,outputs,sampleframes);
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure ProcessRep64Cb(e:PAEffect;inputs,outputs:PPDouble;sampleFrames:Int32);cdecl;
begin
  TVst2Wrapper(e^.Obj).ProcessRep64(inputs,outputs,sampleframes);
end;
{$endif}

{ TVst2Wrapper }

procedure TVst2Wrapper.SetEditor(AEditor:TVst2Editor);
begin
  if Assigned(AEditor) then
  begin
    FEditor:=AEditor;
    Include(FEffect.Flags,effFlagsHasEditor);
  end;
end;

function TVst2Wrapper.CallHost(opcode:TAMOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  if Assigned(FHost) then
    Result:=FHost(@FEffect,Int32(opcode),index,value,ptr,opt)
  else
    Result:=0;
end;

function TVst2Wrapper.CallHost(opcode:TAMOpcodes):IntPtr;
begin
  if Assigned(FHost) then
    Result:=FHost(@FEffect,Int32(opcode),0,0,nil,0)
  else
    Result:=0;
end;

procedure TVst2Wrapper.ProcessEvents(events:PVstEvents);
var
  i:Integer;
begin
  if Assigned(events) then
    with events^ do
    for i:=0 to NumEvents-1 do
      if events[i]^.Typ=kVstMidiType then
        ProcessMidiEvent(PVstMidiEvent(events[i])^)
      else if events[i]^.Typ=kVstSysExType then
        ProcessMidiSysexEvent(PVstMidiSysexEvent(events[i])^);
end;

procedure TVst2Wrapper.EditGetRect(rect:PPERect);
begin
  if Assigned(FEditor) then FEditor.GetRect(rect);
end;

procedure TVst2Wrapper.EditOpen(parent:Pointer);
begin
  if Assigned(FEditor) then FEditor.Open(parent);
end;

procedure TVst2Wrapper.EditClose;
begin
  if Assigned(FEditor) then FEditor.Close;
end;

procedure TVst2Wrapper.EditIdle;
begin
  if Assigned(FEditor) then FEditor.Idle;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVst2Wrapper.EditKeyDown(charactor:Int32;virt,modifier:Byte):Int32;
begin
  if Assigned(FEditor) then
    Result:=Int32(FEditor.OnKeyDown(charactor,virt,modifier))
  else
    Result:=0;
end;

function TVst2Wrapper.EditKeyUp(charactor:Int32;virt,modifier:Byte):Int32;
begin
  if Assigned(FEditor) then
    Result:=Int32(FEditor.OnKeyUp(charactor,virt,modifier))
  else
    Result:=0;
end;

procedure TVst2Wrapper.SetEditKnobMode(mode:Int32);
begin
  if Assigned(FEditor) then FEditor.SetKnobMode(mode);
end;
{$endif}

function TVst2Wrapper.Dispatcher(opcode:TAEffectOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=0;
  case opcode of
    effOpen: Open;
    effClose: Close;
    effSetProgram: SetPreset(value);
    effGetProgram: Result:=GetPreset;
    effSetProgramName: SetPresetName(ptr);
    effGetProgramName: GetPresetName(ptr);
    effGetParamLabel: GetParamLabel(index,ptr);
    effGetParamDisplay: GetParamDisplay(index,ptr);
    effGetParamName: GetParamName(index,ptr);
    effGetVu: ;
    effSetSampleRate: SetSampleRate(opt);
    effSetBlockSize: SetBlockSize(value);
    effMainsChanged: if value<>0 then Resume else Suspend;
    effEditGetRect: EditGetRect(ptr);
    effEditOpen: EditOpen(ptr);
    effEditClose: EditClose;
    effEditDraw: ;
    effEditMouse: ;
    effEditKey: ;
    effEditIdle: EditIdle;
    effEditTop: ;
    effEditSleep: ;
    effIdentify: Result:=MakeLong('NvEf');
    effGetChunk: Result:=GetChunk(ptr,index<>0);
    effSetChunk: Result:=SetChunk(ptr,value,index<>0);
    effProcessEvents: ProcessEvents(ptr);
    effCanBeAutomated: Result:=IntPtr(ParamCanBeAutomated(index));
    effString2Parameter: Result:=IntPtr(String2Parameter(index,ptr));
    effGetNumProgramCategories: ;
    effGetProgramNameIndexed: Result:=IntPtr(GetPresetNameIndexed(index,ptr));
    effCopyProgram: ;
    effConnectInput: ;
    effConnectOutput: ;
    effGetInputProperties: Result:=GetInputProperties(index,ptr);
    effGetOutputProperties: Result:=GetOutputProperties(index,ptr);
    effGetPlugCategory: Result:=IntPtr(GetPlugCategory);
    effGetCurrentPosition: ;
    effGetDestinationBuffer: ;
    effOfflineNotify: OfflineNotify(ptr,value,index<>0);
    effOfflinePrepare: OfflinePrepare(ptr,value);
    effOfflineRun: OfflineRun(ptr,value);
    effProcessVarIO: ProcessVarIO(ptr);
    effSetSpeakerArrangement: SetSpeakerArrangement(FromIntPtr(value),ptr);
    effSetBlockSizeAndSampleRate: ;
    effSetBypass: SetBypass(value<>0);
    effGetEffectName: GetEffectName(ptr);
    effGetErrorText: ;
    effGetVendorString: GetVendorString(ptr);
    effGetProductString: GetProductString(ptr);
    effGetVendorVersion: Result:=GetVendorVersion;
    effVendorSpecific: Result:=VendorSpecific(index,value,ptr,opt);
    effCanDo: Result:=CanDo(ptr);
    effGetTailSize: Result:=GetTailSize;
    effIdle: ;
    effGetIcon: ;
    effSetViewPosition: ;
    effGetParameterProperties: Result:=GetParamProperties(index,ptr);
    effKeysRequired: ;
    effGetVstVersion: Result:=kVstVersion;
{$ifdef VST_2_1_EXTENSIONS}
    effEditKeyDown: Result:=EditKeyDown(index,value,Trunc(opt));
    effEditKeyUp: Result:=EditKeyUp(index,value,Trunc(opt));
    effSetEditKnobMode: SetEditKnobMode(value);
    effGetMidiProgramName: Result:=GetMidiProgramName(index,ptr);
    effGetCurrentMidiProgram: Result:=GetCurrentMidiProgram(index,ptr);
    effGetMidiProgramCategory: Result:=GetMidiProgramCategory(index,ptr);
    effHasMidiProgramsChanged: Result:=IntPtr(HasMidiProgramsChanged(index));
    effGetMidiKeyName: Result:=IntPtr(GetMidiKeyName(index,ptr));
    effBeginSetProgram: BeginSetProgram;
    effEndSetProgram: EndSetProgram;
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement: GetSpeakerArrangement(FromIntPtr(value),ptr);
    effShellGetNextPlugin: Result:=ShellGetNextPlugin(ptr);
    effStartProcess: StartProcess;
    effStopProcess: StopProcess;
    effSetTotalSampleToProcess: SetTotalSampleToProcess(value);
    effSetPanLaw: SetPanLaw(value,opt);
    effBeginLoadBank: Result:=BeginLoadBank(ptr);
    effBeginLoadProgram: Result:=BeginLoadPreset(ptr);
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision: SetProcessPrecision(value);
    effGetNumMidiInputChannels: Result:=GetNumMidiInputChannels;
    effGetNumMidiOutputChannels: Result:=GetNumMidiOutputChannels;
{$endif}
  end;
end;

function TVst2Wrapper.GetParameter(index:Int32):Single;
begin
  Result:=0;
end;

procedure TVst2Wrapper.SetParameter(index:Int32;value:Single);
begin
end;

procedure TVst2Wrapper.Process(const inputs,outputs:TBuffer32;SampleFrames:Int32);
begin
end;

procedure TVst2Wrapper.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
begin
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVst2Wrapper.ProcessRep64(const inputs,outputs:TBuffer64;SampleFrames:Int32);
begin
end;
{$endif}

procedure TVst2Wrapper.Open;
begin
end;

procedure TVst2Wrapper.Close;
begin
end;

procedure TVst2Wrapper.Suspend;
begin
end;

procedure TVst2Wrapper.Resume;
begin
end;

procedure TVst2Wrapper.SetSampleRate(SampleRate:Single);
begin
  FSampleRate:=SampleRate;
end;

procedure TVst2Wrapper.SetBlockSize(BlockSize:Int32);
begin
  FBlockSize:=BlockSize;
end;

procedure TVst2Wrapper.SetPreset(preset:Int32);
begin
  if preset<FNumPresets then
    FCurPreset:=preset;
end;

function TVst2Wrapper.GetPreset:Int32;
begin
  Result:=FCurPreset;
end;

procedure TVst2Wrapper.SetPresetName(name:PAnsiChar);
begin
end;

procedure TVst2Wrapper.GetPresetName(name:PAnsiChar);
begin
  name^:=#0;
end;

function TVst2Wrapper.GetPresetNameIndexed(index:Int32;text:PAnsiChar):Boolean;
begin
  text^:=#0;
  Result:=False;
end;

procedure TVst2Wrapper.GetParamLabel(index:Int32;text:PAnsiChar);
begin
  text^:=#0;
end;

procedure TVst2Wrapper.GetParamDisplay(index:Int32;text:PAnsiChar);
begin
  text^:=#0;
end;

procedure TVst2Wrapper.GetParamName(index:Int32;text:PAnsiChar);
begin
  text^:=#0;
end;

function TVst2Wrapper.GetChunk(Data:PPointer;IsPreset:Boolean):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.SetChunk(Data:Pointer;ByteSize:Int32;IsPreset:Boolean):Int32;
begin
  Result:=0;
end;

procedure TVst2Wrapper.ProcessMidiEvent(const event:TVstMidiEvent);
begin
end;

procedure TVst2Wrapper.ProcessMidiSysexEvent(const event:TVstMidiSysexEvent);
begin
end;

function TVst2Wrapper.ParamCanBeAutomated(index:Int32):Boolean;
begin
  Result:=False;
end;

function TVst2Wrapper.String2Parameter(index:Int32;str:PAnsiChar):Boolean;
begin
  Result:=False;
end;

procedure TVst2Wrapper.SetBypass(OnOff:Boolean);
begin
end;

function TVst2Wrapper.GetPlugCategory:TVstPlugCategory;
begin
  Result:=kPlugCategUnknown;
end;

procedure TVst2Wrapper.GetEffectName(text:PAnsiChar);
begin
end;

procedure TVst2Wrapper.GetVendorString(text:PAnsiChar);
begin
end;

procedure TVst2Wrapper.GetProductString(text:PAnsiChar);
begin
end;

function TVst2Wrapper.GetVendorVersion:Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.VendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=0;
end;

function TVst2Wrapper.CanDo(text:PAnsiChar):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetTailSize:Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetInputProperties(index:Int32;properties:PVstPinProperties):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetOutputProperties(index:Int32;properties:PVstPinProperties):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetParamProperties(index:Int32;properties:PVstParameterProperties):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetMidiProgramName(index:Int32;Midipn:PMidiProgramName):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetCurrentMidiProgram(index:Int32;Midipn:PMidiProgramName):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetMidiProgramCategory(index:Int32;Midipc:PMidiProgramCategory):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.HasMidiProgramsChanged(index:Int32):Boolean;
begin
  Result:=False;
end;

function TVst2Wrapper.GetMidiKeyName(index:Int32;Midikn:PMidiKeyName):Boolean;
begin
  Result:=False;
end;

procedure TVst2Wrapper.BeginSetProgram;
begin
end;

procedure TVst2Wrapper.EndSetProgram;
begin
end;

procedure TVst2Wrapper.SetSpeakerArrangement(input,output:PVstSpeakerArrangement);
begin
end;

procedure TVst2Wrapper.GetSpeakerArrangement(input,output:PVstSpeakerArrangement);
begin
end;

function TVst2Wrapper.ShellGetNextPlugin(name:PAnsiChar):Int32;
begin
  Result:=0;
end;

procedure TVst2Wrapper.StartProcess;
begin
end;

procedure TVst2Wrapper.StopProcess;
begin
end;

procedure TVst2Wrapper.ProcessVarIO(VarIO:PVstVariableIO);
begin
end;

procedure TVst2Wrapper.OfflineNotify(AudioFiles:PVstAudioFile;count:IntPtr;start:Boolean);
begin
end;

procedure TVst2Wrapper.OfflinePrepare(task:PVstOfflineTask;count:IntPtr);
begin
end;

procedure TVst2Wrapper.OfflineRun(task:PVstOfflineTask;count:IntPtr);
begin
end;

procedure TVst2Wrapper.SetTotalSampleToProcess(SampleNumber:IntPtr);
begin
end;

procedure TVst2Wrapper.SetPanLaw(PanLaw:IntPtr;gain:Single);
begin
end;

function TVst2Wrapper.BeginLoadBank(patch:PVstPatchChunkInfo):Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.BeginLoadPreset(patch:PVstPatchChunkInfo):Int32;
begin
  Result:=0;
end;

procedure TVst2Wrapper.SetProcessPrecision(precision:IntPtr);
begin
end;

function TVst2Wrapper.GetNumMidiInputChannels:Int32;
begin
  Result:=0;
end;

function TVst2Wrapper.GetNumMidiOutputChannels:Int32;
begin
  Result:=0;
end;

procedure TVst2Wrapper.SetUniqueID(ID:Int32);
begin
  FEffect.UniqueID:=ID;
end;

procedure TVst2Wrapper.SetVersion(version:Int32);
begin
  FEffect.Version:=version;
end;

procedure TVst2Wrapper.SetNumInput(num:Int32);
begin
  FEffect.NumInputs:=num;
end;

procedure TVst2Wrapper.SetNumOutput(num:Int32);
begin
  FEffect.NumOutputs:=num;
end;

procedure TVst2Wrapper.SetInitialDelay(delay:Int32);
begin
  FEffect.InitialDelay:=delay;
end;

procedure TVst2Wrapper.CanProcessReplacing;
begin
  Include(FEffect.Flags,effFlagsCanReplacing);
end;

procedure TVst2Wrapper.PresetsAreChunk;
begin
  Include(FEffect.Flags,effFlagsProgramChunks);
end;

procedure TVst2Wrapper.SetAsSynth;
begin
  Include(FEffect.Flags,effFlagsIsSynth);
end;

procedure TVst2Wrapper.SetNoTail;
begin
  Include(FEffect.Flags,effFlagsNoSoundInStop);
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVst2Wrapper.CanDoubleReplacing;
begin
  Include(FEffect.Flags,effFlagsCanDoubleReplacing);
end;
{$endif}

constructor TVst2Wrapper.Create(AHost:THostCallback;NumPresets,NumParams:Int32);
begin
  FHost := AHost;
  FSampleRate := 44100;
  FBlockSize := 1024;
  FNumPresets := NumPresets;
  FNumParams := NumParams;
  FCurPreset := 0;
  FEffect.Magic := kEffectMagic;
  FEffect.Dispatcher := @DispatcherCb;
  FEffect.Process := @ProcessCb;
  FEffect.SetParameter := @SetParameterCb;
  FEffect.GetParameter := @GetParameterCb;
  FEffect.NumPrograms := NumPresets;
  FEffect.NumParams := FNumParams;
  FEffect.NumInputs := 2;
  FEffect.NumOutputs := 2;
  FEffect.IORatio := 1;
  FEffect.Obj := self;
  FEffect.UniqueID := MakeLong('NoEf');
  FEffect.Version := 1;
  FEffect.ProcessReplacing := @ProcessRepCb;
{$ifdef VST_2_4_EXTENSIONS}
  Include(FEffect.Flags,effFlagsCanReplacing);
  FEffect.ProcessDoubleReplacing := @ProcessRep64Cb;
{$endif}
end;

destructor TVst2Wrapper.Destroy;
begin
  inherited Destroy;
end;

function TVst2Wrapper.GetEffect:PAEffect;
begin
  Result:=@FEffect;
end;

procedure TVst2Wrapper.SetParamAutomated(index:Int32;value:Single);
begin
  SetParameter(index,value);
  CallHost(amAutomate,index,0,nil,value);
end;

function TVst2Wrapper.GetHostVersion:Int32;
begin
  Result:=CallHost(amVersion);
end;

function TVst2Wrapper.GetCurrentUniqueID:Int32;
begin
  Result:=CallHost(amCurrentId);
end;

procedure TVst2Wrapper.HostIdle;
begin
  CallHost(amIdle);
end;

function TVst2Wrapper.GetTimeInfo(flags:TVstTimeInfoFlags):PVstTimeInfo;
begin
  Result:=FromIntPtr(CallHost(amGetTime,0,IntPtr(flags)));
end;

function TVst2Wrapper.IOChanged:Boolean;
begin
  Result:=CallHost(amIOChanged)<>0;
end;

procedure TVst2Wrapper.HostProcessEvents(events:PVstEvents);
begin
  CallHost(amProcessEvents,0,0,events);
end;

procedure TVst2Wrapper.SizeWindow(Width,Height:Integer);
begin
  CallHost(amSizeWindow,Width,Height);
end;

function TVst2Wrapper.UpdateSampleRate:Single;
var
  res:IntPtr;
begin
  res:=CallHost(amGetSampleRate);
  if res>0 then FSampleRate:=res;
  Result:=FSampleRate;
end;

function TVst2Wrapper.UpdateBlockSize:Int32;
var
  res:IntPtr;
begin
  res:=CallHost(amGetBlockSize);
  if res>0 then FBlockSize:=res;
  Result:=FBlockSize;
end;

function TVst2Wrapper.GetCurrentProcessLevel:Int32;
begin
  Result:=CallHost(amGetCurrentProcessLevel);
end;

function TVst2Wrapper.GetAutomationState:Int32;
begin
  Result:=CallHost(amGetAutomationState);
end;

function TVst2Wrapper.OfflineStart(AudioFiles:PVstAudioFile;AudioFileNum,NewAudioFilesNum:Int32):Boolean;
begin
  Result:=CallHost(amOfflineStart,NewAudioFilesNum,AudioFileNum,AudioFiles)<>0;
end;

function TVst2Wrapper.OfflineRead(task:PVstOfflineTask;option:TVstOfflineOption;ReadSource:Boolean):Boolean;
begin
  Result:=CallHost(amOfflineRead,Int32(ReadSource),IntPtr(option),task)<>0;
end;

function TVst2Wrapper.OfflineWrite(task:PVstOfflineTask;option:TVstOfflineOption):Boolean;
begin
  Result:=CallHost(amOfflineWrite,0,IntPtr(option),task)<>0;
end;

function TVst2Wrapper.OfflineGetCurrentPass:Int32;
begin
  Result:=CallHost(amOfflineGetCurrentPass);
end;

function TVst2Wrapper.OfflineGetCurrentMetaPass:Int32;
begin
  Result:=CallHost(amOfflineGetCurrentMetaPass);
end;

procedure TVst2Wrapper.GetHostVendorString(text:PAnsiChar);
begin
  CallHost(amGetVendorString,0,0,text);
end;

procedure TVst2Wrapper.GetHostProductString(text:PAnsiChar);
begin
  CallHost(amGetProductString,0,0,text);
end;

function TVst2Wrapper.GetHostVendorVersion:Int32;
begin
  Result:=CallHost(amGetVendorVersion);
end;

function TVst2Wrapper.HostVendorSpecific(index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=CallHost(amVendorSpecific,index,value,ptr,opt);
end;

function TVst2Wrapper.HostCanDo(text:PAnsiChar):Boolean;
begin
  Result:=CallHost(amCanDo,0,0,text)=1;
end;

function TVst2Wrapper.GetHostLanguage:Int32;
begin
  Result:=CallHost(amGetLanguage);
end;

function TVst2Wrapper.GetDirectory:PAnsiChar;
begin
  Result:=FromIntPtr(CallHost(amGetDirectory));
end;

function TVst2Wrapper.UpdateDisplay:Boolean;
begin
  Result:=CallHost(amUpdateDisplay)<>0;
end;

function TVst2Wrapper.BeginEdit(index:Int32):Boolean;
begin
  Result:=CallHost(amBeginEdit,index)<>0;
end;

function TVst2Wrapper.EndEdit(index:Int32):Boolean;
begin
  Result:=CallHost(amEndEdit,index)<>0;
end;

function TVst2Wrapper.HostOpenFileSelector(FileSelect:PVstFileSelect):Boolean;
begin
  Result:=CallHost(amOpenFileSelector,0,0,FileSelect)<>0;
end;

function TVst2Wrapper.HostCloseFileSelector(FileSelect:PVstFileSelect):Boolean;
begin
  Result:=CallHost(amCloseFileSelector,0,0,FileSelect)<>0;
end;

{ TVst2Editor }

constructor TVst2Editor.Create(APlugin:TVst2Wrapper);
begin
  FPlugin:=APlugin;
end;

procedure TVst2Editor.GetRect(rect:PPERect);
begin
  if Assigned(rect) then
    rect^:=@FERect;
end;

procedure TVst2Editor.Open(parent:Pointer);
begin
  if Assigned(parent) then
    FParentWindow:=parent;
end;

procedure TVst2Editor.Close;
begin
  FParentWindow:=nil;
end;

procedure TVst2Editor.Idle;
begin
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVst2Editor.OnKeyDown(charactor:Int32;virt,modifier:Byte):Boolean;
begin
  Result:=False;
end;

function TVst2Editor.OnKeyUp(charactor:Int32;virt,modifier:Byte):Boolean;
begin
  Result:=False;
end;

procedure TVst2Editor.SetKnobMode(mode:Int32);
begin
end;
{$endif}

end.

