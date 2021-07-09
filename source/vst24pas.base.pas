{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst24pas.base
// Description : Vst plugin and editor class
// Created by  : PeaZomboss, 2021/5
-------------------------------------------------------------------------------}
unit vst24pas.base;

{$I vst24pas.inc}

interface

uses vst24pas.Core;

type
  TVstEditor = class; // forward

  { TVstPlugin }

  TVstPlugin = class
  protected
    FVSTHost:     TVstHostCallback; // Host callback
    FEditor:      TVstEditor;       // Pointer to the plug-in's editor
    FSampleRate:  single;           // Current sample rate
    FBlockSize:   Int32;            // Maximum block size
    FNumPrograms: Int32;            // Number of programs
    FNumParams:   Int32;            // Number of parameters
    FCurProgram:  Int32;            // Current program
    FCEffect:     TAEffect;         // #TAEffect object
    class function DispatchEffectClass(e: PAEffect; opcode: TAEffectOpcodes; index: Int32;
      Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl; static;
    class function GetParameterClass(e: PAEffect; index: Int32): single; cdecl; static;
    class procedure SetParameterClass(e: PAEffect; index: Int32; Value: single); cdecl; static;
    class procedure ProcessClass(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: Int32); cdecl; static; deprecated;
    class procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: Int32); cdecl; static;

  {$ifdef VST_2_4_EXTENSIONS}
    class procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: Int32); cdecl; static;
  {$endif VST_2_4_EXTENSIONS}

    { Make it easy to set AEffect flags }
    procedure SwitchVstAEffectFlags(Flag: TVstAEffectFlag; switch: boolean); inline;
  public
    constructor Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32); virtual;
    destructor Destroy; override;

    function Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer; opt: single): IntPtr; virtual;

    { Called when plug-in is initialized}
    procedure Open; virtual;
    { Called when plug-in will be released}
    procedure Close; virtual;
    { Called when plug-in is switched to off}
    procedure Suspend; virtual;
    { Called when plug-in is switched to on}
    procedure Resume; virtual;

    { Called when the sample rate changes (always in a suspend state)}
    procedure SetSampleRate(SampleRate: single); virtual;
    { Called when the Maximun block size changes (always in a suspend state).
      Note that the sampleFrames in Process Calls could be smaller than this block size, but NOT bigger.}
    procedure SetBlockSize(BlockSize: Int32); virtual;
    { Process 32 bit (single precision) floats (always in a resume state)}
    procedure ProcessReplacing(const Inputs, Outputs: TBuffer32; SampleFrames: Int32); virtual; abstract;

  {$ifdef VST_2_4_EXTENSIONS}
    { Process 64 bit (double precision) floats (always in a resume state) processReplacing}
    procedure ProcessDoubleReplacing(const Inputs, Outputs: TBuffer64; SampleFrames: Int32); virtual;
  {$endif VST_2_4_EXTENSIONS}

    { Called when a parameter changed}
    procedure SetParameter(index: Int32; Value: single); virtual;
    { Return the value of the parameter with index}
    function GetParameter(index: Int32): single; virtual;
    { Called after a control has changed in the editor and when the associated parameter should be automated}
    procedure SetParameterAutomated(index: Int32; Value: single); virtual;

    { Return the index to the current program}
    function GetProgram: Int32; virtual;
    { Set the current program to program}
    procedure SetProgram(_Program: Int32); virtual;
    { Stuff the name field of the current program with name. Limited to #kVstMaxProgNameLen.}
    procedure SetProgramName(Name: PAnsiChar); virtual;
    { Stuff name with the name of the current program. Limited to #kVstMaxProgNameLen.}
    procedure GetProgramName(Name: PAnsiChar); virtual;
    { Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to #kVstMaxParamStrLen.}
    procedure GetParameterLabel(index: Int32; _Label: PAnsiChar); virtual;
    { Stuff text with a string representation ("0.5", "-3", "PLATE", etc...)
      of the value of parameter  index. Limited to #kVstMaxParamStrLen.}
    procedure GetParameterDisplay(index: Int32; Text: PAnsiChar); virtual;
    { Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to #kVstMaxParamStrLen.}
    procedure GetParameterName(index: Int32; Text: PAnsiChar); virtual;
    { Host stores plug-in state. Returns the size in bytes of the chunk (plug-in allocates the data array)}
    function GetChunk(Data: PPointer; IsPreset: boolean = False): Int32; virtual;
    { Host restores plug-in state}
    function SetChunk(Data: Pointer; ByteSize: Int32; IsPreset: boolean = False): Int32; virtual;

    { Must be called to set the plug-ins unique ID!}
    procedure SetUniqueID(ID: Int32); virtual;
    { Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration,
      this number is the maximun available inputs.}
    procedure SetNumInputs(inputs: Int32); virtual;
    { Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration,
      this number is the maximun available ouputs.}
    procedure SetNumOutputs(outputs: Int32); virtual;

    { Tells that processReplacing() could be used. Mandatory in VST 2.4!}
    procedure CanProcessReplacing(state: boolean = True); virtual;

  {$ifdef VST_2_4_EXTENSIONS}
    { Tells that processDoubleReplacing() is implemented.}
    procedure CanDoubleReplacing(state: boolean = True); virtual;
  {$endif VST_2_4_EXTENSIONS}

    { Program data is handled in formatless chunks (using getChunk-setChunks)}
    procedure ProgramsAreChunks(state: boolean = True); virtual;
    { Use to report the plug-in's latency (Group Delay)}
    procedure SetInitialDelay(delay: Int32); virtual;

    { Should be called if you want to define your own editor}
    procedure SetEditor(editor: TVSTEditor);
    { Returns the attached editor}
    function GetEditor: TVSTEditor; virtual;

    { Returns the pointer of #TAEffect structure}
    function GetAEffect: PAEffect; virtual;
    { Returns the current sample rate}
    function GetSampleRate: single; virtual;
    { Returns the current Maximum block size}
    function GetBlockSize: Int32; virtual;

    { Returns the Host's version (for example 2400 for VST 2.4)}
    function GetMasterVersion: Int32; virtual;
    { Returns current unique identifier when loading shell plug-ins}
    function GetCurrentUniqueId: Int32; virtual;
    { Give idle time to Host application}
    procedure MasterIdle; virtual;

    { Stuffs text with an amplitude on the [0.0, 1.0] scale converted to its value in decibels.}
    procedure dB2String(Value: single; Text: PAnsiChar; MaxLen: Int32=7); virtual;
    { Stuffs text with the frequency in Hertz that has a period of samples.}
    procedure Hz2String(samples: single; Text: PAnsiChar; MaxLen: Int32=7); virtual;
    { Stuffs text with the duration in milliseconds of samples frames.}
    procedure Ms2String(samples: single; Text: PAnsiChar; MaxLen: Int32=7); virtual;
    { Stuffs text with a string representation on the floating point value.}
    procedure Float2String(Value: single; Text: PAnsiChar; MaxLen: Int32=7); virtual;
    { Stuffs text with a string representation on the integer value.}
    procedure Int2String(Value: single; Text: PAnsiChar; MaxLen: Int32=7); virtual;

    { same as processReplace but is deprecated }
    procedure Process(const Inputs, Outputs: TBuffer32; sampleFrames: Int32); virtual; deprecated;
    function GetVu: single; virtual; deprecated;
    procedure HasVu(state: boolean = True); virtual; deprecated;
    procedure HasClip(state: boolean = True); virtual; deprecated;
    procedure CanMono(state: boolean = True); virtual; deprecated;
    procedure SetRealtimeQualities(qualities: Int32); virtual; deprecated;
    procedure SetOfflineQualities(qualities: Int32); virtual; deprecated;
    function IsInputConnected(input: Int32): boolean; virtual; deprecated;
    function IsOutputConnected(output: Int32): boolean; virtual; deprecated;

    { vst2.x extensions }

    { Indicates if a parameter can be automated}
    function CanParameterBeAutomated(index: Int32): boolean; virtual;
    { Convert a string representation to a parameter value}
    function String2parameter(index: Int32; Text: PAnsiChar): boolean; virtual;
    { Return parameter properties}
    function GetParameterProperties(index: Int32; p: PVstParameterProperties): boolean; virtual;

  {$ifdef VST_2_1_EXTENSIONS}
    { To be called before #setParameterAutomated (on Mouse Down). This will be used by the Host for specific Automation Recording.}
    function BeginEdit(index: Int32): boolean; virtual;
    { To be called after #setParameterAutomated (on Mouse Up)}
    function EndEdit(index: Int32): boolean; virtual;
  {$endif VST_2_1_EXTENSIONS}

    { Fill text with name of program index (category deprecated in VST 2.4)}
    function GetProgramNameIndexed(category, index: Int32; Text: PAnsiChar): boolean; virtual;

  {$ifdef VST_2_1_EXTENSIONS}
    { Called before a program is loaded}
    function BeginSetProgram: boolean; virtual;
    { Called after a program was loaded}
    function EndSetProgram: boolean; virtual;
  {$endif VST_2_1_EXTENSIONS}

  {$ifdef VST_2_3_EXTENSIONS}
    { Called before a Bank is loaded.}
    function BeginLoadBank(ptr: PVstPatchChunkInfo): Int32; virtual;
    { Called before a Program is loaded. (called before #beginSetProgram).}
    function BeginLoadProgram(ptr: PVstPatchChunkInfo): Int32; virtual;
  {$endif VST_2_3_EXTENSIONS}

    { Tell Host numInputs and/or numOutputs and/or initialDelay (and/or numParameters: to be avoid) have changed}
    function IOChanged: boolean; virtual;
    { Returns sample rate from Host (may issue setSampleRate())}
    function UpdateSampleRate: double; virtual;
    { Returns block size from Host (may issue getBlockSize())}
    function UpdateBlockSize: Int32; virtual;
    { Returns the Audio (maybe ASIO) input latency values}
    function GetInputLatency: Int32; virtual;
    { Returns the Audio (maybe ASIO) output latency values}
    function GetOutputLatency: Int32; virtual;

    { Return the properties of output index}
    function GetInputProperties(index: Int32; properties: PVstPinProperties): boolean; virtual;
    { Return the properties of input index}
    function GetOutputProperties(index: Int32; properties: PVstPinProperties): boolean; virtual;

    { Set the plug-in's speaker arrangements}
    function SetSpeakerArrangement(PluginInput, PluginOutput: PVstSpeakerArrangement): boolean; virtual;
    { Return the plug-in's speaker arrangements}
    function GetSpeakerArrangement(PluginInput, PluginOutput: PPVstSpeakerArrangement): boolean; virtual;
    { For 'soft-bypass' (this could be automated (in Audio Thread) that why you
      could NOT call iochanged (if needed) in this function, do it in fxidle).}
    function SetBypass(OnOff: boolean): boolean; virtual;

  {$ifdef VST_2_3_EXTENSIONS}
    { Set the Panning Law used by the Host @see VstPanLawType.}
    function SetPanLaw(_type: Int32; val: single): boolean; virtual;
  {$endif VST_2_3_EXTENSIONS}

  {$ifdef VST_2_4_EXTENSIONS}
    { Set floating-point precision used for processing (32 or 64 bit)}
    function SetProcessPrecision(precision: Int32): boolean; virtual;

    { Returns number of MIDI input channels used [0, 16]}
    function GetNumMidiInputChannels: Int32; virtual;
    { Returns number of MIDI output channels used [0, 16]}
    function GetNumMidiOutputChannels: Int32; virtual;
  {$endif VST_2_4_EXTENSIONS}

    { Get time information from Host}
    function GetTimeInfo(filter: Int32): PVstTimeInfo; virtual;
    { Returns the Host's process level}
    function GetCurrentProcessLevel: Int32; virtual;
    { Returns the Host's automation state}
    function GetAutomationState: Int32; virtual;
    { Called when new MIDI events come in}
    function ProcessEvents(events: PVstEvents): Int32; virtual;
    { Send MIDI events back to Host application}
    function SendVstEventsToHost(events: PVstEvents): boolean;

  {$ifdef VST_2_3_EXTENSIONS}
    { Called one time before the start of process call. This indicates that the process call will be interrupted
      (due to Host reconfiguration or bypass state when the plug-in doesn't support softBypass)}
    function StartProcess: Int32; virtual;
    { Called after the stop of process call}
    function StopProcess: Int32; virtual;
  {$endif VST_2_3_EXTENSIONS}

    { Used for variable I/O processing (offline processing like timestreching)}
    function ProcessVariableIo(VarIO: PVstVariableIO): boolean; virtual;

  {$ifdef VST_2_3_EXTENSIONS}
    function SetTotalSampleToProcess(Value: Int32): Int32; virtual;
  {$endif VST_2_3_EXTENSIONS}

    { Fills text with a string identifying the vendor limit 64}
    function GetHostVendorString(Text: PAnsiChar): boolean; virtual;
    { Fills text with a string with product name limit 64}
    function GetHostProductString(Text: PAnsiChar): boolean; virtual;
    { Returns vendor-specific version (for example 3200 for Nuendo 3.2)}
    function GetHostVendorVersion: Int32; virtual;
    { No specific definition}
    function HostVendorSpecific(lArg1, lArg2: Int32; ptrArg: Pointer; floatArg: single): IntPtr; virtual;
    { Reports what the Host is able to do (#hostCanDos)}
    function CanHostDo(Text: PAnsiChar): Int32; virtual;
    { Returns the Host's language (#TVstHostLanguage)}
    function GetHostLanguage: Int32; virtual;

    { Set if plug-in is a synth}
    procedure IsSynth(state: boolean = True); virtual;
    { Plug-in won't produce output signals while there is no input}
    procedure NoTail(state: boolean = True); virtual;
    { Returns tail size; 0 is default (return 1 for 'no tail'), used in offline processing too}
    function GetGetTailSize: Int32; virtual;
    { Returns the plug-in's directory}
    function GetDirectory: Pointer; virtual;
    { Fill text with a string identifying the effect limit 32}
    function GetEffectName(Name: PAnsiChar): boolean; virtual;
    { Fill text with a string identifying the vendor limit 64}
    function GetVendorString(Text: PAnsiChar): boolean; virtual;
    { Fill text with a string identifying the product name limit 64}
    function GetProductString(Text: PAnsiChar): boolean; virtual;
    { Return vendor-specific version}
    function GetVendorVersion: Int32; virtual;
    { No definition, vendor specific handling}
    function VendorSpecific(lArg1, lArg2: Int32; ptrArg: Pointer; floatArg: single): IntPtr; virtual;
    { Reports what the plug-in is able to do (#plugCanDos)}
    function CanDo(Text: PAnsiChar): Int32; virtual;
    { Returns the current VST Version (#kVstVersion)}
    function GetVstVersion: Int32; virtual;
    { Specify a category that fits the plug (#TVstPlugCategory)}
    function GetPlugCategory: TVstPlugCategory; virtual;

  {$ifdef VST_2_1_EXTENSIONS}
    { Fill midiProgramName with information for 'thisProgramIndex'.}
    function GetMidiProgramName(channel: Int32; MidiProgramName: PMidiProgramName): Int32; virtual;
    { Fill currentProgram with information for the current MIDI program.}
    function GetCurrentMidiProgram(channel: Int32; CurrentProgram: PMidiProgramName): Int32; virtual;
    { Fill category with information for 'thisCategoryIndex'.}
    function GetMidiProgramCategory(channel: Int32; category: PMidiProgramName): Int32; virtual;
    { Return true if the #MidiProgramNames, #MidiKeyNames or #MidiControllerNames had changed on this MIDI channel.}
    function HasMidiProgramsChanged(channel: Int32): boolean; virtual;
    { Fill keyName with information for 'thisProgramIndex' and 'thisKeyNumber'}
    function GetMidiKeyName(channel: Int32; keyName: PMidiKeyName): boolean; virtual;
  {$endif VST_2_1_EXTENSIONS}

    { Something has changed in plug-in, request an update display like program (MIDI too) and parameters list in Host}
    function UpdateDisplay: boolean; virtual;
    { Requests to resize the editor window}
    function SizeWindow(Width, Height: Int32): boolean; virtual;

  {$ifdef VST_2_1_EXTENSIONS}
    { Open a Host File selector (see #TVstFileSelect definition)}
    function OpenFileSelector(ptr: PVstFileSelect): boolean; virtual;
  {$endif VST_2_1_EXTENSIONS}

  {$ifdef VST_2_2_EXTENSIONS}
    { Close the Host File selector which was opened by #openFileSelector}
    function CloseFileSelector(ptr: PVstFileSelect): boolean; virtual;
  {$endif VST_2_2_EXTENSIONS}

  {$ifdef VST_2_3_EXTENSIONS}
    { This opcode is only called, if the plug-in is of type #kPlugCategShell, in order to extract all included sub-plugin´s names.}
    function GetNextShellPlugin(Name: PAnsiChar): Int32; virtual;
  {$endif VST_2_3_EXTENSIONS}

  {$ifdef VST_2_3_EXTENSIONS}
    { Allocate memory for a #TVstSpeakerArrangement}
    function AllocateArrangement(arrangement: PPVstSpeakerArrangement; NumChannels: Int32): boolean; virtual;
    { Delete/free memory for an allocated speaker arrangement}
    function DeallocateArrangement(arrangement: PPVstSpeakerArrangement): boolean; virtual;
    { Copy properties from to _to}
    function CopySpeaker(_to, from: PVstSpeakerProperties): boolean; virtual;
    { "_to" is deleted, then created and initialized with the same values as "from" ones ("from" must exist).}
    function MatchArrangement(_to: PPVstSpeakerArrangement; from: PVstSpeakerArrangement): boolean; virtual;
  {$endif VST_2_3_EXTENSIONS}

    function OfflineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: boolean = True): boolean; virtual;
    function OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): boolean; virtual;
    function OfflineStart(audioFiles: PVstAudioFile; NumAudioFiles, NumNewAudioFiles: Int32): boolean; virtual;
    function OfflineGetCurrentPass: Int32; virtual;
    function OfflineGetCurrentMetaPass: Int32; virtual;
    function OfflineNotify(ptr: PVstAudioFile; NumAudioFiles: Int32; start: boolean): boolean; virtual;
    function OfflinePrepare(offline: PVstOfflineTask; Count: Int32): boolean; virtual;
    function OfflineRun(offline: PVstOfflineTask; Count: Int32): boolean; virtual;
    function OfflineGetNumPasses: Int32; virtual;
    function OfflineGetNumMetaPasses: Int32; virtual;

    { Deprecated }

    procedure WantEvents(filter: Int32 = 1); virtual; deprecated;
    function TempoAt(pos: Int32): Int32; virtual; deprecated;
    function GetNumAutomatableParameters: Int32; virtual; deprecated;
    function GetParameterQuantization: Int32; virtual; deprecated;
    function GetNumCategories: Int32; virtual; deprecated;
    function CopyProgram(destination: Int32): boolean; virtual; deprecated;
    function NeedIdle: boolean; virtual; deprecated;
    function GetPreviousPlug(input: Int32): PAEffect; virtual; deprecated;
    function GetNextPlug(input: Int32): PAEffect; virtual; deprecated;
    procedure InputConnected(index: Int32; state: boolean); virtual; deprecated;
    procedure OutputConnected(index: Int32; state: boolean); virtual; deprecated;
    function WillProcessReplacing: Int32; virtual; deprecated;
    procedure WantAsyncOperation(state: boolean = True); virtual; deprecated;
    procedure HasExternalBuffer(state: boolean = True); virtual; deprecated;
    function ReportCurrentPosition: Int32; virtual; deprecated;
    function ReportDestinationBuffer: PSingle; virtual; deprecated;
    procedure SetOutputSamplerate(samplerate: single); virtual; deprecated;
    function GetInputSpeakerArrangement: PVstSpeakerArrangement; virtual; deprecated;
    function GetOutputSpeakerArrangement: PVstSpeakerArrangement; virtual; deprecated;
    function OpenWindow(window: PVstWindow): Pointer; virtual; deprecated;
    function CloseWindow(window: PVstWindow): boolean; virtual; deprecated;
    procedure SetBlockSizeAndSampleRate(BlockSize: Int32; SampleRate: single); virtual; deprecated;
    function GetErrorText(Text: PAnsiChar): boolean; virtual; deprecated;
    function GetIcon: Pointer; virtual; deprecated;
    function SetViewPosition(x, y: Int32): boolean; virtual; deprecated;
    function FxIdle: Int32; virtual; deprecated;
    function KeysRequired: boolean; virtual; deprecated;

  {$ifdef VST_2_2_EXTENSIONS}
    { Returns in platform format the path of the current chunk (could be called in #setChunk ()) (FSSpec on MAC else char*)}
    function GetChunkFile(NativePath: Pointer): boolean; virtual; deprecated;
  {$endif VST_2_2_EXTENSIONS}
  end;

  { TVstEditor }

  TVstEditor = class
  protected
    FPlugin:       TVstPlugin; // associated effect instance
    FParentWindow: Pointer;    // platform-dependent parent window (HWND or WindowRef)
  public
    { Editor class constructor. Requires pointer to associated plugin instance.}
    constructor Create(Plugin: TVstPlugin = nil);
    destructor Destroy; override;
    { Returns associated effect instance}
    function GetPlugin: TVstPlugin; virtual;
    { Query editor size as #TERect}
    function GetRect(rect: PPERect): boolean; virtual;
    { Open editor, pointer to parent windows is platform-dependent (HWND on Windows, WindowRef on Mac).}
    function Open(ptr: Pointer): boolean; virtual;
    { Close editor (detach from parent window)}
    procedure Close; virtual;
    { Returns true if editor is currently open}
    function IsOpen: boolean; virtual;
    { Idle call supplied by Host application}
    procedure Idle; virtual;

  {$ifdef TARGET_API_MAC_CARBON}
    { nothing }
  {$endif}

  {$ifdef VST_2_1_EXTENSIONS}
    { Receive key down event. Return true only if key was really used!}
    function OnKeyDown(var keyCode: TVstKeyCode): boolean; virtual;
    { Receive key up event. Return true only if key was really used!}
    function OnKeyUp(var keyCode: TVstKeyCode): boolean; virtual;
    { Handle mouse wheel event, distance is positive or negative to indicate wheel direction.}
    function OnWheel(distance: single): boolean; virtual;
    { Set knob mode (if supported by Host). See CKnobMode in VSTGUI.}
    function SetKnobMode(val: Int32): boolean; virtual;
  {$endif VST_2_1_EXTENSIONS}
  end;

implementation

uses
  SysUtils, Math;

{ TVstEditor }

constructor TVstEditor.Create(Plugin: TVstPlugin);
begin
  FPlugin := Plugin;
  FParentWindow := nil;
end;

destructor TVstEditor.Destroy;
begin
  inherited Destroy;
end;

function TVstEditor.GetPlugin: TVstPlugin;
begin
  Result := FPlugin;
end;

function TVstEditor.GetRect(rect: PPERect): boolean;
begin
  rect^  := nil;
  Result := False;
end;

function TVstEditor.Open(ptr: Pointer): boolean;
begin
  FParentWindow := ptr;
  Result := False;
end;

procedure TVstEditor.Close;
begin
  FParentWindow := nil;
end;

function TVstEditor.IsOpen: boolean;
begin
  Result := Assigned(FParentWindow);
end;

procedure TVstEditor.Idle;
begin
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVstEditor.OnKeyDown(var keyCode: TVstKeyCode): boolean;
begin
  Result := False;
end;

function TVstEditor.OnKeyUp(var keyCode: TVstKeyCode): boolean;
begin
  Result := False;
end;

function TVstEditor.OnWheel(distance: single): boolean;
begin
  Result := False;
end;

function TVstEditor.SetKnobMode(val: Int32): boolean;
begin
  Result := False;
end;
{$endif VST_2_1_EXTENSIONS}

{ TVstPlugin }

class function TVstPlugin.DispatchEffectClass(e: PAEffect; opcode: TAEffectOpcodes; index: Int32;
  Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
var
  v: TVstPlugin;
begin
  v := TVstPlugin(e^._object);
  if opcode = effClose then
  begin
    v.Dispatcher(opcode, index, Value, ptr, opt);
    FreeAndNil(v);
    Exit(1);
  end;
  Result := v.Dispatcher(opcode, index, Value, ptr, opt);
end;

class function TVstPlugin.GetParameterClass(e: PAEffect; index: Int32): single; cdecl;
begin
  Result := TVstPlugin(e^._object).GetParameter(index);
end;

class procedure TVstPlugin.SetParameterClass(e: PAEffect; index: Int32; Value: single); cdecl;
begin
  TVstPlugin(e^._object).SetParameter(index, Value);
end;

class procedure TVstPlugin.ProcessClass(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVstPlugin(e^._object).Process(inputs, outputs, sampleFrames);
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
  TVstPlugin(e^._Object).Process(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}


class procedure TVstPlugin.ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVstPlugin(e^._object).processReplacing(inputs, outputs, sampleFrames);
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
  TVstPlugin(e^._Object).ProcessReplacing(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}

{$ifdef VST_2_4_EXTENSIONS}
class procedure TVstPlugin.ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: Int32); cdecl;
{$ifdef FPC}
begin
  TVstPlugin(e^._object).processDoubleReplacing(inputs, outputs, sampleFrames);
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
  TVstPlugin(e^._Object).processDoubleReplacing(InputsArr, OutputsArr, SampleFrames);
end;
{$endif}
{$endif VST_2_4_EXTENSIONS}

procedure TVstPlugin.SwitchVstAEffectFlags(Flag: TVstAEffectFlag; switch: boolean);
begin
  if switch then
    Include(FCEffect.Flags, Flag)
  else
    Exclude(FCEffect.Flags, Flag);
end;

constructor TVstPlugin.Create(VstHost: TVstHostCallback; NumPrograms, NumParams: Int32);
begin
  FVSTHost := VstHost;
  FEditor  := nil;
  FSampleRate := 44100;
  FBlockSize := 1024;
  FNumPrograms := NumPrograms;
  FNumParams := NumParams;
  FCurProgram := 0;
  FCEffect.Magic := kEffectMagic;
  FCEffect.Dispatcher := @DispatchEffectClass;
  FCEffect.Process := @ProcessClass; // Though is deprecated
  FCEffect.SetParameter := @SetParameterClass;
  FCEffect.GetParameter := @GetParameterClass;
  FCEffect.NumPrograms := FNumPrograms;
  FCEffect.NumParams := FNumParams;
  FCEffect.NumInputs := 2;  // stereo input
  FCEffect.NumOutputs := 2; // stereo output
  FCEffect.IORatio := 1;
  FCEffect._Object := self;
  FCEffect.UniqueID := CCONST('N', 'o', 'E', 'f');
  FCEffect.Version := 1;
  FCEffect.ProcessReplacing := @ProcessClassReplacing;
{$ifdef VST_2_4_EXTENSIONS}
  CanProcessReplacing(); // mandatory in VST 2.4!
  FCEffect.ProcessDoubleReplacing := @ProcessClassDoubleReplacing;
{$endif}
end;

destructor TVstPlugin.Destroy;
begin
  if Assigned(FEditor) then
    FreeAndNil(FEditor);
  inherited Destroy;
end;

function TVstPlugin.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr;
  ptr: Pointer; opt: single): IntPtr;
{$ifdef VST_2_1_EXTENSIONS}
var
  KeyCode: TVstKeyCode;
{$endif}
begin
  Result := 0; // default

  case opcode of
    effOpen: Open;   // open the plugin
    effClose: Close; // close the plugin
    effSetProgram:
      if Value < FNumPrograms then
        setProgram(Value);
    effGetProgram: Result := getProgram;
    effSetProgramName: setProgramName(ptr);
    effGetProgramName: getProgramName(ptr);
    effGetParamLabel: getParameterLabel(index, ptr);
    effGetParamDisplay: getParameterDisplay(index, ptr);
    effGetParamName: getParameterName(index, ptr);

    effSetSampleRate: SetSampleRate(opt);
    effSetBlockSize: setBlockSize(Value);
    effMainsChanged:
      if Value = 0 then
        Suspend
      else
        Resume;
  {$ifndef VST_FORCE_DEPRECATED}
    effGetVu: Result := Trunc(getVu * 32767); // deprecated
  {$endif VST_FORCE_DEPRECATED}

    //---Editor------------
    effEditGetRect:
      if Assigned(FEditor) then
        Result := IntPtr(FEditor.GetRect(ptr));
    effEditOpen:
      if Assigned(FEditor) then
        Result := IntPtr(FEditor.Open(ptr));
    effEditClose:
      if Assigned(FEditor) then
        FEditor.Close;
    effEditIdle:
      if Assigned(FEditor) then
        FEditor.idle;

  {$if defined(TARGET_API_MAC_CARBON) and not defined(VST_FORCE_DEPRECATED)}
    { nothing }
  {$endif}

    effIdentify: Result := CCONST('N', 'v', 'E', 'f'); // deprecated

    //---Persistence-------
    effGetChunk: Result := GetChunk(ptr, index <> 0);
    effSetChunk: Result := SetChunk(ptr, Value, index <> 0);

    { vst2.x extension }

    //---VstEvents----------------------
    effProcessEvents: Result := processEvents(ptr);

    //---Parameters and Programs----------------------
    effCanBeAutomated: Result := IntPtr(canParameterBeAutomated(index));
    effString2Parameter: Result := IntPtr(string2parameter(index, ptr));

    effGetProgramNameIndexed: Result := IntPtr(getProgramNameIndexed(Int32(Value), index, ptr));
  {$ifndef VST_FORCE_DEPRECATED}
    effGetNumProgramCategories: Result := getNumCategories; // deprecated
    effCopyProgram: Result := Int32(copyProgram(index)); // deprecated

    //---Connections, Configuration----------------------
    effConnectInput: // deprecated
    begin
      inputConnected(index, Value <> 0);
      Result := 1;
    end;
    effConnectOutput: // deprecated
    begin
      outputConnected(index, Value <> 0);
      Result := 1;
    end;
  {$endif VST_FORCE_DEPRECATED}

    effGetInputProperties: Result := IntPtr(GetInputProperties(index, ptr));
    effGetOutputProperties: Result := IntPtr(GetOutputProperties(index, ptr));
    effGetPlugCategory: Result := IntPtr(GetPlugCategory);

  {$ifndef VST_FORCE_DEPRECATED}
    //---Realtime----------------------
    effGetCurrentPosition: Result := reportCurrentPosition; // deprecated

    effGetDestinationBuffer: Result := ToIntPtr(reportDestinationBuffer); // deprecated
  {$endif VST_FORCE_DEPRECATED}

    //---Offline----------------------
    effOfflineNotify: Result := IntPtr(offlineNotify(ptr, Value, index <> 0));
    effOfflinePrepare: Result := IntPtr(offlinePrepare(ptr, Value));
    effOfflineRun: Result := IntPtr(offlineRun(ptr, Value));

    //---Others----------------------
    effSetSpeakerArrangement: Result := IntPtr(setSpeakerArrangement(FromIntPtr(Value), ptr));
    effProcessVarIo: Result := IntPtr(processVariableIo(ptr));
  {$ifndef VST_FORCE_DEPRECATED}
    effSetBlockSizeAndSampleRate: // deprecated
    begin
      setBlockSizeAndSampleRate(Value, opt);
      Result := 1;
    end;
  {$endif VST_FORCE_DEPRECATED}
    effSetBypass: Result := IntPtr(setBypass(Value <> 0));
    effGetEffectName: Result := IntPtr(getEffectName(ptr));
    effGetVendorString: Result := IntPtr(getVendorString(ptr));
    effGetProductString: Result := IntPtr(getProductString(ptr));
    effGetVendorVersion: Result := GetVendorVersion;
    effVendorSpecific: Result := VendorSpecific(index, Value, ptr, opt);
    effCanDo: Result := CanDo(ptr);

    effGetTailSize: Result := GetGetTailSize;

  {$ifndef VST_FORCE_DEPRECATED}
    effGetErrorText: Result := IntPtr(getErrorText(ptr)); // deprecated

    effGetIcon: Result := ToIntPtr(getIcon); // deprecated

    effSetViewPosition: Result := IntPtr(setViewPosition(index, Value)); // deprecated

    effIdle: Result := fxIdle; // deprecated

    effKeysRequired: Result := IntPtr(keysRequired); // deprecated
  {$endif VST_FORCE_DEPRECATED}

    effGetParameterProperties: Result := IntPtr(GetParameterProperties(index, ptr));

    effGetVstVersion: Result := GetVstVersion;

    //---Others----------------------
  {$ifdef VST_2_1_EXTENSIONS}
    effEditKeyDown:
      if Assigned(FEditor) then
      begin
        KeyCode.character := index;
        KeyCode.modifier := byte(Value);
        KeyCode.virt := byte(Trunc(opt));
        Result := IntPtr(FEditor.onKeyDown(KeyCode));
      end;

    effEditKeyUp:
      if Assigned(FEditor) then
      begin
        KeyCode.character := index;
        KeyCode.modifier := byte(Value);
        KeyCode.virt := byte(Trunc(opt));
        Result := IntPtr(FEditor.onKeyUp(KeyCode));
      end;

    effSetEditKnobMode:
      if Assigned(FEditor) then
        Result := IntPtr(FEditor.setKnobMode(Value));

    effGetMidiProgramName: Result := GetMidiProgramName(index, ptr);
    effGetCurrentMidiProgram: Result := GetCurrentMidiProgram(index, ptr);
    effGetMidiProgramCategory: Result := GetMidiProgramCategory(index, ptr);
    effHasMidiProgramsChanged: Result := IntPtr(HasMidiProgramsChanged(index));
    effGetMidiKeyName: Result := IntPtr(GetMidiKeyName(index, ptr));
    effBeginSetProgram: Result := IntPtr(BeginSetProgram);
    effEndSetProgram: Result := IntPtr(EndSetProgram);
  {$endif VST_2_1_EXTENSIONS}

  {$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement: Result := IntPtr(GetSpeakerArrangement(FromIntPtr(Value), ptr));

    effSetTotalSampleToProcess: Result := SetTotalSampleToProcess(Value);

    effShellGetNextPlugin: Result := GetNextShellPlugin(ptr);

    effStartProcess: Result := StartProcess;
    effStopProcess: Result  := StopProcess;

    effSetPanLaw: Result := IntPtr(SetPanLaw(Value, opt));

    effBeginLoadBank: Result := BeginLoadBank(ptr);
    effBeginLoadProgram: Result := BeginLoadProgram(ptr);
  {$endif VST_2_3_EXTENSIONS}

  {$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision: Result := IntPtr(SetProcessPrecision(Value));

    effGetNumMidiInputChannels: Result := GetNumMidiInputChannels;

    effGetNumMidiOutputChannels: Result := GetNumMidiOutputChannels;
  {$endif VST_2_4_EXTENSIONS}

      //---unknown-----------------
    else;
  end;
end;

procedure TVstPlugin.Open;
begin
end;

procedure TVstPlugin.Close;
begin
end;

procedure TVstPlugin.Suspend;
begin
end;

procedure TVstPlugin.Resume;
begin
  { If this effect is a synth or can receive midi events,
    we call the deprecated wantEvents() as some host rely on it.}
  if (canDo(CanDoReceiveVstMidiEvent) = 1) or (effFlagsIsSynth in FCEffect.flags) then
    wantEvents;
end;

procedure TVstPlugin.SetSampleRate(SampleRate: single);
begin
  FSampleRate := SampleRate;
end;

procedure TVstPlugin.SetBlockSize(BlockSize: Int32);
begin
  FBlockSize := BlockSize;
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVstPlugin.ProcessDoubleReplacing(const Inputs, Outputs: TBuffer64; SampleFrames: Int32);
begin
end;
{$endif VST_2_4_EXTENSIONS}

procedure TVstPlugin.SetParameter(index: Int32; Value: single);
begin
end;

function TVstPlugin.GetParameter(index: Int32): single;
begin
  Result := 0;
end;

procedure TVstPlugin.SetParameterAutomated(index: Int32; Value: single);
begin
  SetParameter(index, Value);
  if Assigned(FVSTHost) then
    FVSTHost(@FCEffect, amAutomate, index, 0, nil, Value);
end;

function TVstPlugin.GetProgram: Int32;
begin
  Result := FCurProgram;
end;

procedure TVstPlugin.SetProgram(_Program: Int32);
begin
  FCurProgram := _program;
end;

procedure TVstPlugin.SetProgramName(Name: PAnsiChar);
begin
end;

procedure TVstPlugin.GetProgramName(Name: PAnsiChar);
begin
  Name^ := #0;
end;

procedure TVstPlugin.GetParameterLabel(index: Int32; _Label: PAnsiChar);
begin
  _Label^ := #0;
end;

procedure TVstPlugin.GetParameterDisplay(index: Int32; Text: PAnsiChar);
begin
  Text^ := #0;
end;

procedure TVstPlugin.GetParameterName(index: Int32; Text: PAnsiChar);
begin
  Text^ := #0;
end;

function TVstPlugin.GetChunk(Data: PPointer; IsPreset: boolean): Int32;
begin
  Result := 0;
end;

function TVstPlugin.SetChunk(Data: Pointer; ByteSize: Int32; IsPreset: boolean): Int32;
begin
  Result := 0;
end;

procedure TVstPlugin.SetUniqueID(ID: Int32);
begin
  FCEffect.uniqueID := ID;
end;

procedure TVstPlugin.SetNumInputs(inputs: Int32);
begin
  FCEffect.numInputs := inputs;
end;

procedure TVstPlugin.SetNumOutputs(outputs: Int32);
begin
  FCEffect.numOutputs := outputs;
end;

procedure TVstPlugin.CanProcessReplacing(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsCanReplacing, state);
end;

{$ifdef VST_2_4_EXTENSIONS}
procedure TVstPlugin.CanDoubleReplacing(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsCanDoubleReplacing, state);
end;
{$endif VST_2_4_EXTENSIONS}

procedure TVstPlugin.ProgramsAreChunks(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsProgramChunks, state);
end;

procedure TVstPlugin.SetInitialDelay(delay: Int32);
begin
  FCEffect.initialDelay := delay;
end;

procedure TVstPlugin.SetEditor(editor: TVSTEditor);
begin
  FEditor := editor;
  SwitchVstAEffectFlags(effFlagsHasEditor, Assigned(FEditor));
end;

function TVstPlugin.GetEditor: TVSTEditor;
begin
  Result := FEditor;
end;

function TVstPlugin.GetAEffect: PAEffect;
begin
  Result := @FCEffect;
end;

function TVstPlugin.GetSampleRate: single;
begin
  Result := FSampleRate;
end;

function TVstPlugin.GetBlockSize: Int32;
begin
  Result := FBlockSize;
end;

function TVstPlugin.GetMasterVersion: Int32;
begin
  Result := 1;
  if Assigned(FVSTHost) then
  begin
    Result := FVSTHost(@FCEffect, amVersion, 0, 0, nil, 0);
    if Result = 0 then
      Result := 1;
  end;
end;

function TVstPlugin.GetCurrentUniqueId: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amCurrentId, 0, 0, nil, 0);
end;

procedure TVstPlugin.MasterIdle;
begin
  if Assigned(FVSTHost) then
    FVSTHost(@FCEffect, amIdle, 0, 0, nil, 0);
end;

procedure TVstPlugin.dB2String(Value: single; Text: PAnsiChar; MaxLen: Int32);
begin
  if Value <= 0 then
    vststrncpy(Text, '-Inf', maxlen)
  else
    float2string(20 * Log10(Value), Text, maxlen);
end;

procedure TVstPlugin.Hz2String(samples: single; Text: PAnsiChar; MaxLen: Int32);
begin
  if samples = 0 then
    float2string(0, Text, maxlen)
  else
    float2string(0.5 * GetSampleRate / samples, Text, maxlen);
end;

procedure TVstPlugin.Ms2String(samples: single; Text: PAnsiChar; MaxLen: Int32);
begin
  float2string(samples * 1000 / GetSampleRate, Text, maxlen);
end;

procedure TVstPlugin.Float2String(Value: single; Text: PAnsiChar; MaxLen: Int32);
var
  c, neg: Int32;
  str: array[0..31] of AnsiChar;
  s: PAnsiChar;
  v, integ, i10, mantissa, m10, ten: double;
begin
  // there's no better way
  c := 0;
  neg := 0;
  ten := 10;

  v := Value;
  if v < 0 then
  begin
    neg := 1;
    Value := -Value;
    v := -v;
    c := c + 1;
    if v > 9999999 then
    begin
      vststrncpy(Text, 'Huge!', maxlen);
      exit;
    end;
  end else if v > 99999999 then
  begin
    vststrncpy(Text, 'Huge!', maxlen);
    exit;
  end;

  s  := PAnsiChar(@str) + 31;
  s^ := #0;
  Dec(s);
  s^ := '.';
  Dec(s);
  c := c + 1;

  integ := Floor(v);
  i10 := FMod(integ, ten);
  s^  := AnsiChar(Trunc(i10) + Ord('0'));
  Dec(s);
  integ := integ / ten;
  c  := c + 1;
  while (integ >= 1) and (c < 8) do
  begin
    i10 := fmod(integ, ten);
    s^  := AnsiChar(Trunc(i10) + Ord('0'));
    Dec(s);
    integ := integ / ten;
    c := c + 1;
  end;
  if neg = 1 then
  begin
    s^ := '-';
    Dec(s);
  end;
  vststrncpy(Text, s + 1, maxlen);
  if c >= 8 then
    exit;

  s  := PAnsiChar(@str) + 31;
  s^ := #0;
  Dec(s);
  mantissa := fmod(v, 1);
  mantissa := mantissa * power(ten, 8 - c);
  while c < 8 do
  begin
    if mantissa <= 0 then
    begin
      s^ := '0';
      Dec(s);
    end else
    begin
      m10 := FMod(mantissa, ten);
      s^  := AnsiChar(Trunc(m10) + Ord('0'));
      Dec(s);
      mantissa := mantissa / 10;
    end;
    c := c + 1;
  end;
  vststrncat(Text, s + 1, maxlen);
end;

procedure TVstPlugin.Int2String(Value: single; Text: PAnsiChar; MaxLen: Int32);
begin
  if (Value >= 100000000) or (Value <= -10000000) then
  begin
    vststrncpy(Text, 'Huge!', maxlen);
    exit;
  end;
  vstStrncpy(Text, PAnsiChar(IntToStr(Trunc(Value))), MaxLen);
end;

procedure TVstPlugin.Process(const Inputs, Outputs: TBuffer32; sampleFrames: Int32);
begin
end;

function TVstPlugin.GetVu: single;
begin
  Result := 0;
end;

procedure TVstPlugin.HasVu(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsHasVu, state);
end;

procedure TVstPlugin.HasClip(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsHasClip, state);
end;

procedure TVstPlugin.CanMono(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsCanMono, state);
end;

procedure TVstPlugin.SetRealtimeQualities(qualities: Int32);
begin
  FCEffect.realQualities := qualities;
end;

procedure TVstPlugin.SetOfflineQualities(qualities: Int32);
begin
  FCEffect.offQualities := qualities;
end;

function TVstPlugin.IsInputConnected(input: Int32): boolean;
var
  ret: Int32;
begin
  ret := 0;
  if Assigned(FVSTHost) then
    ret  := FVSTHost(@FCEffect, amPinConnected, input, 0, nil, 0);
  Result := ret = 0; // return value is 0 for true
end;

function TVstPlugin.IsOutputConnected(output: Int32): boolean;
var
  ret: Int32;
begin
  ret := 0;
  if Assigned(FVSTHost) then
    ret  := FVSTHost(@FCEffect, amPinConnected, output, 1, nil, 0);
  Result := ret = 0; // return value is 0 for true
end;

{ vst2.x extensions }

function TVstPlugin.CanParameterBeAutomated(index: Int32): boolean;
begin
  Result := True;
end;

function TVstPlugin.String2parameter(index: Int32; Text: PAnsiChar): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetParameterProperties(index: Int32; p: PVstParameterProperties): boolean;
begin
  Result := False;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVstPlugin.BeginEdit(index: Int32): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amBeginEdit, index, 0, nil, 0) <> 0;
end;

function TVstPlugin.EndEdit(index: Int32): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amEndEdit, index, 0, nil, 0) <> 0;
end;
{$endif VST_2_1_EXTENSIONS}

function TVstPlugin.GetProgramNameIndexed(category, index: Int32; Text: PAnsiChar): boolean;
begin
  Result := False;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVstPlugin.BeginSetProgram: boolean;
begin
  Result := False;
end;

function TVstPlugin.EndSetProgram: boolean;
begin
  Result := False;
end;
{$endif VST_2_1_EXTENSIONS}

{$ifdef VST_2_3_EXTENSIONS}
function TVstPlugin.BeginLoadBank(ptr: PVstPatchChunkInfo): Int32;
begin
  Result := 0;
end;

function TVstPlugin.BeginLoadProgram(ptr: PVstPatchChunkInfo): Int32;
begin
  Result := 0;
end;
{$endif VST_2_3_EXTENSIONS}

function TVstPlugin.IOChanged: boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amIOChanged, 0, 0, nil, 0) <> 0;
end;

function TVstPlugin.UpdateSampleRate: double;
var
  res: IntPtr;
begin
  if Assigned(FVSTHost) then
  begin
    res := FVSTHost(@FCEffect, amGetSampleRate, 0, 0, nil, 0);
    if res > 0 then
      FSampleRate := res;
  end;
  Result := FSampleRate;
end;

function TVstPlugin.UpdateBlockSize: Int32;
var
  res: IntPtr;
begin
  if Assigned(FVSTHost) then
  begin
    res := FVSTHost(@FCEffect, amGetBlockSize, 0, 0, nil, 0);
    if res > 0 then
      fblockSize := res;
  end;
  Result := fblockSize;
end;

function TVstPlugin.GetInputLatency: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetInputLatency, 0, 0, nil, 0);
end;

function TVstPlugin.GetOutputLatency: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetOutputLatency, 0, 0, nil, 0);
end;

function TVstPlugin.GetInputProperties(index: Int32; properties: PVstPinProperties): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetOutputProperties(index: Int32; properties: PVstPinProperties): boolean;
begin
  Result := False;
end;

function TVstPlugin.SetSpeakerArrangement(PluginInput, PluginOutput: PVstSpeakerArrangement): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetSpeakerArrangement(PluginInput, PluginOutput: PPVstSpeakerArrangement): boolean;
begin
  pluginInput^ := nil;
  pluginOutput^ := nil;
  Result := False;
end;

function TVstPlugin.SetBypass(OnOff: boolean): boolean;
begin
  Result := False;
end;

{$ifdef VST_2_3_EXTENSIONS}
function TVstPlugin.SetPanLaw(_type: Int32; val: single): boolean;
begin
  Result := False;
end;
{$endif VST_2_3_EXTENSIONS}

{$ifdef VST_2_4_EXTENSIONS}
function TVstPlugin.SetProcessPrecision(precision: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetNumMidiInputChannels: Int32;
begin
  Result := 0;
end;

function TVstPlugin.GetNumMidiOutputChannels: Int32;
begin
  Result := 0;
end;
{$endif VST_2_4_EXTENSIONS}

function TVstPlugin.GetTimeInfo(filter: Int32): PVstTimeInfo;
var
  ret: IntPtr;
begin
  Result := nil;
  if Assigned(FVSTHost) then
  begin
    ret := FVSTHost(@FCEffect, amGetTime, 0, filter, nil, 0);
    Result := FromIntPtr(ret);
  end;
end;

function TVstPlugin.GetCurrentProcessLevel: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetCurrentProcessLevel, 0, 0, nil, 0);
end;

function TVstPlugin.GetAutomationState: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetAutomationState, 0, 0, nil, 0);
end;

function TVstPlugin.ProcessEvents(events: PVstEvents): Int32;
begin
  Result := 0;
end;

function TVstPlugin.SendVstEventsToHost(events: PVstEvents): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amProcessEvents, 0, 0, events, 0) = 0;
end;

{$ifdef VST_2_3_EXTENSIONS}
function TVstPlugin.StartProcess: Int32;
begin
  Result := 0;
end;

function TVstPlugin.StopProcess: Int32;
begin
  Result := 0;
end;
{$endif VST_2_3_EXTENSIONS}

function TVstPlugin.ProcessVariableIo(VarIO: PVstVariableIO): boolean;
begin
  Result := False;
end;

{$ifdef VST_2_3_EXTENSIONS}
function TVstPlugin.SetTotalSampleToProcess(Value: Int32): Int32;
begin
  Result := Value;
end;
{$endif VST_2_3_EXTENSIONS}

function TVstPlugin.GetHostVendorString(Text: PAnsiChar): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetVendorString, 0, 0, Text, 0) <> 0;
end;

function TVstPlugin.GetHostProductString(Text: PAnsiChar): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetProductString, 0, 0, Text, 0) <> 0;
end;

function TVstPlugin.GetHostVendorVersion: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetVendorVersion, 0, 0, nil, 0);
end;

function TVstPlugin.HostVendorSpecific(lArg1, lArg2: Int32; ptrArg: Pointer; floatArg: single): IntPtr;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amVendorSpecific, lArg1, lArg2, ptrArg, floatArg);
end;

function TVstPlugin.CanHostDo(Text: PAnsiChar): Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := Int32(FVSTHost(@FCEffect, amCanDo, 0, 0, Text, 0) <> 0);
end;

function TVstPlugin.GetHostLanguage: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetLanguage, 0, 0, nil, 0);
end;

procedure TVstPlugin.IsSynth(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsIsSynth, state);
end;

procedure TVstPlugin.NoTail(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsNoSoundInStop, state);
end;

function TVstPlugin.GetGetTailSize: Int32;
begin
  Result := 0;
end;

function TVstPlugin.GetDirectory: Pointer;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amGetDirectory, 0, 0, nil, 0));
end;

function TVstPlugin.GetEffectName(Name: PAnsiChar): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetVendorString(Text: PAnsiChar): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetProductString(Text: PAnsiChar): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetVendorVersion: Int32;
begin
  Result := 0;
end;

function TVstPlugin.VendorSpecific(lArg1, lArg2: Int32; ptrArg: Pointer; floatArg: single): IntPtr;
begin
  Result := 0;
end;

function TVstPlugin.CanDo(Text: PAnsiChar): Int32;
begin
  Result := 0;
end;

function TVstPlugin.GetVstVersion: Int32;
begin
  Result := kVstVersion;
end;

function TVstPlugin.GetPlugCategory: TVstPlugCategory;
begin
  Result := kPlugCategUnknown;
  if effFlagsIsSynth in FCEffect.flags then
    Result := kPlugCategSynth;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVstPlugin.GetMidiProgramName(channel: Int32; MidiProgramName: PMidiProgramName): Int32;
begin
  Result := 0;
end;

function TVstPlugin.GetCurrentMidiProgram(channel: Int32; CurrentProgram: PMidiProgramName): Int32;
begin
  Result := -1;
end;

function TVstPlugin.GetMidiProgramCategory(channel: Int32; category: PMidiProgramName): Int32;
begin
  Result := 0;
end;

function TVstPlugin.HasMidiProgramsChanged(channel: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetMidiKeyName(channel: Int32; keyName: PMidiKeyName): boolean;
begin
  Result := False;
end;
{$endif VST_2_1_EXTENSIONS}

function TVstPlugin.UpdateDisplay: boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amUpdateDisplay, 0, 0, nil, 0) <> 0;
end;

function TVstPlugin.SizeWindow(Width, Height: Int32): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amSizeWindow, Width, Height, nil, 0) <> 0;
end;

{$ifdef VST_2_1_EXTENSIONS}
function TVstPlugin.OpenFileSelector(ptr: PVstFileSelect): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) and Assigned(ptr) then
    Result := FVSTHost(@FCEffect, amOpenFileSelector, 0, 0, ptr, 0) <> 0;
end;
{$endif VST_2_1_EXTENSIONS}

{$ifdef VST_2_2_EXTENSIONS}
function TVstPlugin.CloseFileSelector(ptr: PVstFileSelect): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) and Assigned(ptr) then
    Result := FVSTHost(@FCEffect, amCloseFileSelector, 0, 0, ptr, 0) <> 0;
end;
{$endif VST_2_2_EXTENSIONS}

{$ifdef VST_2_3_EXTENSIONS}
function TVstPlugin.GetNextShellPlugin(Name: PAnsiChar): Int32;
begin
  Result := 0;
end;

function TVstPlugin.AllocateArrangement(arrangement: PPVstSpeakerArrangement; NumChannels: Int32): boolean;
var
  ptr: Pointer;
  size: Int32;
begin
  if Assigned(arrangement^) then
    Freemem(arrangement^);

  size := 2 * sizeof(Int32) + NumChannels * sizeof(TVstSpeakerProperties);
  ptr  := AllocMem(size);
  if not Assigned(ptr) then
    Exit(False);

  arrangement^ := ptr;
  arrangement^^.numChannels := NumChannels;
  Result := True;
end;

function TVstPlugin.DeallocateArrangement(arrangement: PPVstSpeakerArrangement): boolean;
begin
  if Assigned(arrangement^) then
  begin
    Freemem(arrangement^);
    arrangement^ := nil;
  end;
  Result := True;
end;

function TVstPlugin.CopySpeaker(_to, from: PVstSpeakerProperties): boolean;
begin
  Result := False;
  if Assigned(from) and Assigned(_to) then
  begin
    vststrncpy(_to^.Name, from^.Name, 63);
    _to^._type  := from^._type;
    _to^.azimuth := from^.azimuth;
    _to^.elevation := from^.elevation;
    _to^.radius := from^.radius;
    _to^.reserved := from^.reserved;
    Move(from^.future, _to^.future, 28); // memcpy (to->future, from->future, 28);
    Result := True;
  end;
end;

function TVstPlugin.MatchArrangement(_to: PPVstSpeakerArrangement; from: PVstSpeakerArrangement): boolean;
var
  i: integer;
begin
  if not Assigned(from) then
    Exit(False);

  if (not deallocateArrangement(_to)) or (not allocateArrangement(_to, from^.numChannels)) then
    Exit(False);

  _to^^._type := from^._type;
  for i := 0 to _to^^.numChannels do
    if not copySpeaker(@_to^^.speakers[i], @from^.speakers[i]) then
      Exit(False);

  Result := True;
end;
{$endif VST_2_3_EXTENSIONS}

function TVstPlugin.OfflineRead(offline: PVstOfflineTask; option: TVstOfflineOption; readSource: boolean): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amOfflineRead, Int32(readSource), Int32(option),
      offline, 0) <> 0;
end;

function TVstPlugin.OfflineWrite(offline: PVstOfflineTask; option: TVstOfflineOption): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amOfflineWrite, 0, Int32(option), offline, 0) <> 0;
end;

function TVstPlugin.OfflineStart(audioFiles: PVstAudioFile; NumAudioFiles, NumNewAudioFiles: Int32): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amOfflineStart, numNewAudioFiles, numAudioFiles,
      audioFiles, 0) <> 0;
end;

function TVstPlugin.OfflineGetCurrentPass: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := Int32(FVSTHost(@FCEffect, amOfflineGetCurrentPass, 0, 0, nil, 0) <> 0);
end;

function TVstPlugin.OfflineGetCurrentMetaPass: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := Int32(FVSTHost(@FCEffect, amOfflineGetCurrentMetaPass, 0, 0, nil, 0) <> 0);
end;

function TVstPlugin.OfflineNotify(ptr: PVstAudioFile; NumAudioFiles: Int32; start: boolean): boolean;
begin
  Result := False;
end;

function TVstPlugin.OfflinePrepare(offline: PVstOfflineTask; Count: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.OfflineRun(offline: PVstOfflineTask; Count: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.OfflineGetNumPasses: Int32;
begin
  Result := 0;
end;

function TVstPlugin.OfflineGetNumMetaPasses: Int32;
begin
  Result := 0;
end;

procedure TVstPlugin.WantEvents(filter: Int32);
begin
  if Assigned(FVSTHost) then
    FVSTHost(@FCEffect, amWantMidi, 0, filter, nil, 0);
end;

function TVstPlugin.TempoAt(pos: Int32): Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := Int32(FVSTHost(@FCEffect, amTempoAt, 0, pos, nil, 0));
end;

function TVstPlugin.GetNumAutomatableParameters: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetNumAutomatableParameters, 0, 0, nil, 0);
end;

function TVstPlugin.GetParameterQuantization: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amGetParameterQuantization, 0, 0, nil, 0);
end;

function TVstPlugin.GetNumCategories: Int32;
begin
  Result := 1;
end;

function TVstPlugin.CopyProgram(destination: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.NeedIdle: boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amNeedIdle, 0, 0, nil, 0) <> 0;
end;

function TVstPlugin.GetPreviousPlug(input: Int32): PAEffect;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amGetPreviousPlug, 0, 0, nil, 0));
end;

function TVstPlugin.GetNextPlug(input: Int32): PAEffect;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amGetNextPlug, 0, 0, nil, 0));
end;

procedure TVstPlugin.InputConnected(index: Int32; state: boolean);
begin
end;

procedure TVstPlugin.OutputConnected(index: Int32; state: boolean);
begin
end;

function TVstPlugin.WillProcessReplacing: Int32;
begin
  Result := 0;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amWillReplaceOrAccumulate, 0, 0, nil, 0);
end;

procedure TVstPlugin.WantAsyncOperation(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsExtIsAsync, state);
end;

procedure TVstPlugin.HasExternalBuffer(state: boolean);
begin
  SwitchVstAEffectFlags(effFlagsExtHasBuffer, state);
end;

function TVstPlugin.ReportCurrentPosition: Int32;
begin
  Result := 0;
end;

function TVstPlugin.ReportDestinationBuffer: PSingle;
begin
  Result := nil;
end;

procedure TVstPlugin.SetOutputSamplerate(samplerate: single);
begin
  if Assigned(FVSTHost) then
    FVSTHost(@FCEffect, amSetOutputSampleRate, 0, 0, nil, samplerate);
end;

function TVstPlugin.GetInputSpeakerArrangement: PVstSpeakerArrangement;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amGetInputSpeakerArrangement, 0, 0, nil, 0));
end;

function TVstPlugin.GetOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amGetOutputSpeakerArrangement, 0, 0, nil, 0));
end;

function TVstPlugin.OpenWindow(window: PVstWindow): Pointer;
begin
  Result := nil;
  if Assigned(FVSTHost) then
    Result := FromIntPtr(FVSTHost(@FCEffect, amOpenWindow, 0, 0, window, 0));
end;

function TVstPlugin.CloseWindow(window: PVstWindow): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) then
    Result := FVSTHost(@FCEffect, amCloseWindow, 0, 0, window, 0) <> 0;
end;

procedure TVstPlugin.SetBlockSizeAndSampleRate(BlockSize: Int32; SampleRate: single);
begin
  FBlockSize  := BlockSize;
  FSampleRate := SampleRate;
end;

function TVstPlugin.GetErrorText(Text: PAnsiChar): boolean;
begin
  Result := False;
end;

function TVstPlugin.GetIcon: Pointer;
begin
  Result := nil;
end;

function TVstPlugin.SetViewPosition(x, y: Int32): boolean;
begin
  Result := False;
end;

function TVstPlugin.FxIdle: Int32;
begin
  Result := 0;
end;

function TVstPlugin.KeysRequired: boolean;
begin
  Result := False;
end;

{$ifdef VST_2_2_EXTENSIONS}
function TVstPlugin.GetChunkFile(NativePath: Pointer): boolean;
begin
  Result := False;
  if Assigned(FVSTHost) and Assigned(nativePath) then
    Result := FVSTHost(@FCEffect, amGetChunkFile, 0, 0, nativePath, 0) <> 0;
end;
{$endif VST_2_2_EXTENSIONS}

end.
