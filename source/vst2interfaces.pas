{-------------------------------------------------------------------------------
//
// Unit name   : vst2interfaces
// Description : Basic structures
// Created by  : PeaZomboss, 2021/07
-------------------------------------------------------------------------------}
unit vst2interfaces;

{$I vcompiler.inc}

interface

{$push}{$A8}{$Z1}

const
// VST version
{$if defined (VST_2_4_EXTENSIONS)}
  kVstVersion = 2400;
{$elseif defined (VST_2_3_EXTENSIONS)}
  kVstVersion = 2300;
{$elseif defined (VST_2_2_EXTENSIONS)}
  kVstVersion = 2200;
{$elseif defined (VST_2_1_EXTENSIONS)}
  kVstVersion = 2100;
{$else}
  kVstVersion = 2;
{$endif}

  // AEffect magic number
  kEffectMagic = $56737450;

type
  // hostCanDos strings Plug-in
  THostCanDos = record
  const
    CanDoSendVstEvents  = 'sendVstEvents'; // Host supports send of Vst events to plug-in
    CanDoSendVstMidiEvent = 'sendVstMidiEvent'; // Host supports send of MIDI events to plug-in
    CanDoSendVstTimeInfo = 'sendVstTimeInfo'; // Host supports send of VstTimeInfo to plug-in
    CanDoReceiveVstEvents = 'receiveVstEvents'; // Host can receive Vst events from plug-in
    CanDoReceiveVstMidiEvent = 'receiveVstMidiEvent'; // Host can receive MIDI events from plug-in
    // Host will indicates the plug-in when something change in plug-in's
    // routing/connections with suspend/resume/setSpeakerArrangement
    CanDoReportConnectionChanges = 'reportConnectionChanges';
    CanDoAcceptIOChanges = 'acceptIOChanges'; // Host supports ioChanged ()
    CanDoSizeWindow = 'sizeWindow'; // used by VSTGUI
    CanDoOffline = 'offline'; // Host supports offline feature
    CanDoOpenFileSelector = 'openFileSelector'; // Host supports function openFileSelector ()
    CanDoCloseFileSelector = 'closeFileSelector'; // Host supports function closeFileSelector ()
    CanDoStartStopProcess = 'startStopProcess'; // Host supports functions startProcess () and stopProcess ()
    // 'shell' handling via uniqueID.
    // If supported by the Host and the Plug-in has the category kPlugCategShell
    CanDoShellCategory = 'shellCategory';
    CanDoSendVstMidiEventFlagIsRealtime = 'sendVstMidiEventFlagIsRealtime'; // Host supports flags for TVstMidiEvent
  end;

  // plugCanDos strings Host
  TPlugCanDos = record
  const
    CanDoSendVstEvents = 'sendVstEvents'; // plug-in will send Vst events to Host
    CanDoSendVstMidiEvent = 'sendVstMidiEvent'; // plug-in will send MIDI events to Host
    CanDoReceiveVstEvents = 'receiveVstEvents'; // plug-in can receive MIDI events from Host
    CanDoReceiveVstMidiEvent = 'receiveVstMidiEvent'; // plug-in can receive MIDI events from Host
    CanDoReceiveVstTimeInfo = 'receiveVstTimeInfo'; // plug-in can receive Time info from Host
    CanDoOffline = 'offline'; // plug-in supports offline functions (offlineNotify, offlinePrepare, offlineRun)
    CanDoBypass = 'bypass'; // plug-in supports function setBypass ()
    CanDoMidiProgramNames = 'midiProgramNames'; // plug-in supports function getMidiProgramName ()
    // Found in FL Studio 20
    CanDoSupportsViewDpiScaling = 'supportsViewDpiScaling'; // plug-in's editor support DPI scaling
  end;

  // Basic types

  PPSingle   = ^PSingle;
  PPDouble   = ^PDouble;

{$ifndef FPC}
  PInt32  = ^Int32;
  PIntPtr = ^IntPtr;
  { Array for process }
  TArrSingle  = array[0..0] of single;
  PArrSingle  = ^TArrSingle;
  TArrDouble  = array[0..0] of double;
  PArrDouble  = ^TArrDouble;
  TArrPSingle = array of PArrSingle;
  TArrPDouble = array of PArrDouble;
  TBuffer32 = TArrPSingle;
  TBuffer64 = TArrPDouble;
{$else}
  TBuffer32 = PPSingle;
  TBuffer64 = PPDouble;
{$endif}

  // Basic dispatcher Opcodes (Host to Plug-in)
  TAEffectOpcodes = (
    effOpen,       // open the plugin, usually be called after loaded the plugin
    effClose,      // close the plugin, usually be called before unloaded the plugin
    effSetProgram, // [value]: new program number, set preset by number
    effGetProgram, // [return value]: current program number, the index of preset which used
    {[ptr]: sz with new program name, limited to 24. Set new name for current preset }
    effSetProgramName,
    {[ptr]: sz buf for current program name, limited to 24. Fill buf with current preset's name }
    effGetProgramName,
    {[ptr]: sz for parameter label, limited to 8 [index]: index of parameter.
     Get param's label to show, e.g. dB, % ... }
    effGetParamLabel,
    {[ptr]: sz for parameter display, limited to 8 [index]: index of parameter. Show the param }
    effGetParamDisplay,
    {[ptr]: sz for parameter name, limited to 8 [index]: index of parameter.
     Get param's name to show, e.g. Gain, Frequency ... }
    effGetParamName,
    effGetVu, // deprecated in VST 2.4. unknown
    effSetSampleRate, // [opt]: new sample rate for audio processing
    effSetBlockSize, // [value]: new maximum block size for audio processing
    effMainsChanged, // [value]: 0 means "turn off", 1 means "turn on". Tell the plugin on or off
    //<If value=1 and the plugin can recerve midi, should call amWantMidi because as some host rely on it
    effEditGetRect, // [ptr]: PPERect receiving pointer to editor size. Get editor's rect, see TERect
    effEditOpen, // [ptr]: system dependent Window pointer, e.g. HWND on Windows. Open(show) the editor
    effEditClose, // close the editor. Usually simply hide the editor.
    effEditDraw,  // deprecated in VST 2.4. unknown
    effEditMouse, // deprecated in VST 2.4. unknown
    effEditKey,   // deprecated in VST 2.4. unknown
    effEditIdle,  // editor idle. Tell the plugin do something it like
    effEditTop,   // deprecated in VST 2.4. unknown
    effEditSleep, // deprecated in VST 2.4. unknown
    effIdentify,  // deprecated in VST 2.4. unknown
    effGetChunk, // [ptr]: PPointer for chunk data address [index]: 0 for bank, 1 for program.
                 //<[return value] size in bytes of the chunk (plug-in allocates the data array).
                 //<Host stores plug-in state.
    effSetChunk, // [ptr]: chunk data [value]: byte size [index]: 0 for bank, 1 for program
                 //<Host restores plug-in state.
    //effNumOpcodes, // Number of VST 1.0 opcodes

    // VST 2.x extensions

    effProcessEvents = Ord(effSetChunk) + 1, // [ptr]: PVstEvents. Called when new MIDI events come in
    {[index]: parameter index [return value]: 1=true, 0=false. Whether the parameter can be automated }
    effCanBeAutomated,
    {[index]: parameter index [ptr]: parameter string [return value]: true for success.
     Not used most of the time, try string value to parameter value(float) }
    effString2Parameter,
    effGetNumProgramCategories, // deprecated. unknown
    {[index]: program index [value]: category, deprecated, set -1
     [ptr]: buf for program name, limited to 24 [return value]: 1 for success.
     Get the name of preset by index }
    effGetProgramNameIndexed,
    effCopyProgram,   // deprecated. unknown
    effConnectInput,  // deprecated. unknown
    effConnectOutput, // deprecated. unknown
    {[index]: input index [ptr]: PVstPinProperties [return value]: 1 if supported. unknown }
    effGetInputProperties,
    {[index]: output index [ptr]: PVstPinProperties [return value]: 1 if supported. unknown }
    effGetOutputProperties,
    effGetPlugCategory, // [return value]: category in TVstPlugCategory
    effGetCurrentPosition,   // deprecated. unknown
    effGetDestinationBuffer, // deprecated. unknown
    {[ptr]: TVstAudioFile array [value]: count [index]: start flag in TVstAudioFileFlags. unknown }
    effOfflineNotify,
    {[ptr]: TVstOfflineTask array [value]: count. unknown }
    effOfflinePrepare,
    {[ptr]: TVstOfflineTask array [value]: count. unknown }
    effOfflineRun,
    effProcessVarIO, // [ptr]: PVstVariableIO. Used for variable I/O processing (offline processing like timestreching), unknown
    {[value]: input PVstSpeakerArrangement [ptr]: output PVstSpeakerArrangement.
     Set the plug-in's speaker arrangements, unknown }
    effSetSpeakerArrangement,
    effSetBlockSizeAndSampleRate, // deprecated [value]: blocksize [opt]: samplerate. Set both blocksize and samplerate
    effSetBypass, // [value]: 1 = bypass, 0 = no bypass. unknown
                  //<For 'soft-bypass' (this could be automated (in Audio Thread) that why you
                  //<could NOT call iochanged (if needed) in this function, do it in fxidle).
    effGetEffectName, // [ptr]: buffer for effect name, limited to 32. Get plugin's name
    effGetErrorText, // deprecated. unknown
    effGetVendorString, // [ptr]: buffer for effect vendor string, limited to 64. Get vendor name
    effGetProductString, // [ptr]: buffer for effect vendor string, limited to 64. Get product name
    effGetVendorVersion, // [return value]: vendor-specific version.
    effVendorSpecific, // no definition, vendor specific handling. 4 params are defined by vendor
    effCanDo, // [ptr]: "can do" string [return value]: 0: "don't know" -1: "no" 1: "yes". See THostCanDos & TPlugCanDos
    {[return value]: tail size (for example the reverb time of a reverb plug-in); 0 is default (return 1 for 'no tail')}
    effGetTailSize,
    effIdle,            // deprecated. unknown
    effGetIcon,         // deprecated. unknown
    effSetViewPosition, // deprecated. unknown
    {[index]: parameter index [ptr]: PVstParameterProperties [return value]: 1 if supported }
    effGetParameterProperties,
    effKeysRequired,    // deprecated. unknown
    effGetVstVersion // [return value]: VST version. Simply return kVstVersion
{$ifdef VST_2_1_EXTENSIONS}
    , effEditKeyDown {
      [index]: ASCII character [value]: virtual key [opt]: modifiers
      [return value]: 1 if key used. }
    , effEditKeyUp {
      [index]: ASCII character [value]: virtual key [opt]: modifiers
      [return value]: 1 if key used. }
    , effSetEditKnobMode {
      [value]: knob mode 0: circular, 1: circular relativ, 2: linear (CKnobMode in VSTGUI). }
    , effGetMidiProgramName {
      [index]: MIDI channel [ptr]: PMidiProgramName
      [return value]: number of used programs, 0 if unsupported.
      Fill midiProgramName with information for 'thisProgramIndex' }
    , effGetCurrentMidiProgram {
      [index]: MIDI channel [ptr]: PMidiProgramName
      [return value]: index of current program.
      Fill currentProgram with information for the current MIDI program }
    , effGetMidiProgramCategory {
      [index]: MIDI channel [ptr]: PMidiProgramCategory
      [return value]: number of used categories, 0 if unsupported.
      Fill category with information for 'thisCategoryIndex' }
    , effHasMidiProgramsChanged {
      [index]: MIDI channel
      [return value]: 1 if the TMidiProgramName(s) or TMidiKeyName(s) have changed.
      Return 1 if the MidiProgramNames, MidiKeyNames or MidiControllerNames had changed on this MIDI channel }
    , effGetMidiKeyName {
      [index]: MIDI channel [ptr]: PMidiKeyName [return value]: 1 if supported, false otherwise.
      Fill keyName with information for 'thisProgramIndex' and 'thisKeyNumber' }
    , effBeginSetProgram // Called before a program is loaded
    , effEndSetProgram   // Called after a program was loaded
{$endif VST_2_1_EXTENSIONS}

{$ifdef VST_2_3_EXTENSIONS}
    , effGetSpeakerArrangement {
      [value]: input PVstSpeakerArrangement [ptr]: output PVstSpeakerArrangement.
      Return the plug-in's speaker arrangements, unknown }
    , effShellGetNextPlugin {
      [ptr]: buffer for plug-in name, limited to 64 [return value]: next plugin's uniqueID.
      This opcode is only called, if the plug-in is of type kPlugCategShell,
      in order to extract all included sub-plugin's names}
    , effStartProcess { unknown
      Called one time before the start of process call. This indicates that the process call will be interrupted
      (due to Host reconfiguration or bypass state when the plug-in doesn't support softBypass) }
    , effStopProcess  // Called after the stop of process call, unknown
    , effSetTotalSampleToProcess // [value]: number of samples to process, offline only!  unknown
    , effSetPanLaw // [value]: pan law [opt]: gain. Set the Panning Law used by the Host, see TVstPanLawType
    , effBeginLoadBank {
      [ptr]: PVstPatchChunkInfo
      [return value]: -1: bank can't be loaded, 1: bank can be loaded, 0: unsupported.
      Called before a Bank is loaded. }
    , effBeginLoadProgram {
      [ptr]: PVstPatchChunkInfo
      [return value]: -1: prog can't be loaded, 1: prog can be loaded, 0: unsupported.
      Called before a Program is loaded. (called before effBeginSetProgram) }
{$endif VST_2_3_EXTENSIONS}

{$ifdef VST_2_4_EXTENSIONS}
    , effSetProcessPrecision // [value]: TVstProcessPrecision.
                             //<Set floating-point precision used for processing (32 or 64 bit)
    , effGetNumMidiInputChannels  // [return value]: number of used MIDI input channels (1-15)
    , effGetNumMidiOutputChannels // [return value]: number of used MIDI output channels (1-15)
{$endif VST_2_4_EXTENSIONS} );

  // Basic dispatcher Opcodes (Plug-in to Host)
  TAudioMasterOpcodes = (
    amAutomate, // [index]: parameter index [opt]: parameter value. Tell host the indexed param should be audomated
    amVersion,  // [return value]: Host VST version (for example 2400 for VST 2.4)
    amCurrentId, // [return value]: current unique identifier on shell plug-in. unknown
    amIdle, // Give idle time to Host application
    amPinConnected, // deprecated in VST 2.4 r2.
    //<[index] input [value] 0 [return value] 0 if connected
    //<[index] output [value] 1 [return value] 0 if connected

    // VST 2.x dispatcher Opcodes (Plug-in to Host)

    amWantMidi = Ord(amPinConnected) + 2, // deprecated. Tell host want to receive midi
    {[return value]: PVstTimeInfo or null if not supported
     [value]: request mask see TVstTimeInfoFlags.
     Get time information from Host }
    amGetTime,
    amProcessEvents, // [ptr]: PVstEvents. Send MIDI events back to Host application. See TVstEvents
    amSetTime, // deprecated. unknown for any
    amTempoAt, // deprecated. [index]: position [return value]: maybe tempo
    amGetNumAutomatableParameters, // deprecated. [return value]: maybe number of automatable parameters
    amGetParameterQuantization,    // deprecated. [return value]: maybe quantization
    amIOChanged, // [return value]: 1 if supported.
    //<Tell Host numInputs and/or numOutputs and/or initialDelay (and/or numParameters: to be avoid) have changed
    amNeedIdle, // deprecated. [return value]: maybe 1 if success. Tell host need idle time
    amSizeWindow, // [index]: new width [value]: new height [return value]: 1 if supported.
                  //<Requests to resize the editor window
    amGetSampleRate, // [return value]: current sample rate. Get sample rate from host
    amGetBlockSize,  // [return value]: current block size. Get block size from host
    amGetInputLatency, // [return value]: input latency in audio samples. Get audio input latency values
    amGetOutputLatency, // [return value]: output latency in audio samples. Get audio output latency values
    amGetPreviousPlug, // deprecated. [return value]: PAEffect of previous plugin
    amGetNextPlug,     // deprecated. [return value]: PAEffect of next plugin
    amWillReplaceOrAccumulate, // deprecated. [return value]: unknown
    amGetCurrentProcessLevel, // [return value]: current process level. See TVstProcessLevels
    amGetAutomationState, // [return value]: current automation state. See TVstAutomationStates
    amOfflineStart, // [index]: numNewAudioFiles [value]: numAudioFiles [ptr]: PVstAudioFile
                    // [return value]: maybe 1 if success. unknown
    {[index]: bool readSource [value]: PVstOfflineOption, see TVstOfflineOption
     [ptr]: PVstOfflineTask, see TVstOfflineTask [return value]: maybe 1 if success. unknown}
    amOfflineRead,
    amOfflineWrite, // [return value]: maybe 1 if success. See amOfflineRead, unknown
    amOfflineGetCurrentPass, // [return value]: maybe 1 if success. unknown
    amOfflineGetCurrentMetaPass, // [return value]: maybe 1 if success. unknown
    amSetOutputSampleRate, // deprecated. [opt]: sample rate.
    amGetOutputSpeakerArrangement, // deprecated. [return value] PVstSpeakerArrangement.
    amGetVendorString, // [ptr]: sz for vendor string, limited to 64 [return value] maybe 1 if success
    amGetProductString, // [ptr]: sz for vendor string, limited to 64 [return value] maybe 1 if success
    amGetVendorVersion, // [return value]: vendor-specific version
    amVendorSpecific, // no definition, vendor specific handling, see effVendorSpecific
    amSetIcon, // deprecated. unknown for any
    amCanDo, // [ptr]: "can do" string [return value]: 1 for supported. Get host CanDos, See THostCanDos
    amGetLanguage, // [return value]: language code, see TVstHostLanguage
    amOpenWindow,  // deprecated. [ptr]: PVstWindow [return value]: maybe pointer to the window
    amCloseWindow, // deprecated. [ptr]: PVstWindow [return value]: maybe 1 for success
    amGetDirectory, // [return value]: FSSpec on MAC, else PAnsiChar. Returns the plug-in's directory
    amUpdateDisplay, // Something has changed in plug-in, request an update display like
                     //<program (MIDI too) and parameters list in Host
                     //<[return value] maybe 1 for success
    amBeginEdit, // [index]: parameter index. [return value]: maybe 1 for success.
                 //<To be called before amAutomate (on Mouse Down).
                 //<This will be used by the Host for specific Automation Recording
    amEndEdit, // [index]: parameter index. [return value]: maybe 1 for success.
               //<To be called after amAutomate (on Mouse Up)
    amOpenFileSelector, // [ptr]: PVstFileSelect [return value]: 1 if supported. Open a Host File selector
    amCloseFileSelector, // [ptr]: PVstFileSelect [return value]: maybe 1 if success.
                         //<Close the Host File selector which was opened by amOpenFileSelector
    amEditFile, // deprecated. unknown for any
    // Returns in platform format the path of the current chunk (could be called in effSetChunk)
    // (FSSpec on MAC else PChar)
    // [ptr]: char[2048] or sizeof(FSSpec) [return value]: 1 if supported
    amGetChunkFile, // deprecated
    amGetInputSpeakerArrangement // deprecated. [return value]: PVstSpeakerArrangement
  );

  TVstAEffectFlag  = (
    effFlagsHasEditor, // 1<<0 set if the plug-in provides a custom editor
    effFlagsHasClip,   // 1<<1 deprecated
    effFlagsHasVu,     // 1<<2 deprecated
    effFlagsCanMono,   // 1<<3 deprecated
    effFlagsCanReplacing,  // 1<<4 supports replacing process mode (which should the default mode in VST 2.4)
    effFlagsProgramChunks, // 1<<5 program data is handled in formatless chunks
    effFlagsIsSynth=8,     // 1<<8  plug-in is a synth (VSTi), Host may assign mixer channels for its outputs
    effFlagsNoSoundInStop, // 1<<9  plug-in does not produce sound when input is all silence
    effFlagsExtIsAsync,    // 1<<10 deprecated
    effFlagsExtHasBuffer   // 1<<11 deprecated
{$ifdef VST_2_4_EXTENSIONS}
    , effFlagsCanDoubleReplacing // 1<<12 plug-in supports double precision processing
{$endif} );
  // AEffect flags
  TVstAEffectFlags = set of TVstAEffectFlag;

  PAEffect = ^TAEffect; // forward

  // Implemented by host, called by plugin, use opcodes in TAudioMasterOpcodes
  AudioMasterCallback = function(effect: PAEffect; opcode: TAudioMasterOpcodes; index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
  // Implemented by plugin, called by host, use opcodes in TAEffectOpcodes
  AEffectDispatcherProc = function(effect: PAEffect; opcode: TAEffectOpcodes; index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
  AEffectProcessProc = procedure(effect: PAEffect; inputs, outputs: PPSingle; sampleFrames: Int32); cdecl;
  AEffectProcessDoubleProc = procedure(effect: PAEffect; inputs, outputs: PPDouble; sampleFrames: Int32); cdecl;
  AEffectSetParameterProc = procedure(effect: PAEffect; index: Int32; parameter: single); cdecl;
  AEffectGetParameterProc = function(effect: PAEffect; index: Int32): single; cdecl;

  { set some alias }

  THostCallback   = AudioMasterCallback;
  TAEDispatcherCb = AEffectDispatcherProc;
  TAEProcess32Cb    = AEffectProcessProc;
  TAEProcess64Cb  = AEffectProcessDoubleProc;
  TAESetParamCb   = AEffectSetParameterProc;
  TAEGetParamCb   = AEffectGetParameterProc;

  // Audio effect structure define
  TAEffect = record
    Magic:         Int32;  // must be kEffectMagic
    Dispatcher:    TAEDispatcherCb; // Host to Plug-in dispatcher
    Process:       TAEProcess32Cb; // deprecated, default process callback function before 2.4
    SetParameter:  TAESetParamCb; // Set new value of automatable parameter
    GetParameter:  TAEGetParamCb; // Returns current value of automatable parameter
    NumPrograms:   Int32;  // number of programs, or we can say presets
    NumParams:     Int32;  // number of parameters, all parameter are included in preset
    NumInputs:     Int32;  // number of audio inputs
    NumOutputs:    Int32;  // number of audio outputs
    Flags:         TVstAEffectFlags; // See TVstAEffectFlags
    Resvd1:        IntPtr; // reserved for Host, must be 0
    Resvd2:        IntPtr; // reserved for Host, must be 0
    { For algorithms which need input in the first place(Group delay or latency in Samples).
      This value should be initialized in a resume state. }
    InitialDelay:  Int32;
    RealQualities: Int32;   // deprecated unused member
    OffQualities:  Int32;   // deprecated unused member
    IORatio:       single;  // deprecated unused member
    pObject:       Pointer; // The plugin class pointer, see vst2pluginbase unit
    User:          Pointer; // user-defined pointer
    { Registered unique identifier(register it at Steinberg 3rd party support Web).
      This is used to identify a plug-in during save+load of preset and project. }
    UniqueID:      Int32;
    Version:       Int32; // plug-in version (example 1100 for version 1.1.0.0)
    ProcessReplacing: TAEProcess32Cb; // Process audio samples in replacing mode, default in 2.4
{$ifdef VST_2_4_EXTENSIONS}
    // Process double-precision audio samples in replacing mode, optional in 2.4
    ProcessDoubleReplacing: TAEProcess64Cb;
    Future: array[0..55] of byte; // reserved for future use (please zero)
{$else}
    Future: array[0..59] of byte; // reserved for future use (please zero)
{$endif}
  end;

const
  { VST String length limits (in characters excl. 0 byte)
    Maybe useless because many hosts have bigger buffer than the limits
    But it is better to set it smaller than 32, even 16 for shorter ones }

  kVstMaxParamStrLen   = 8;  // used for effGetParamLabel, effGetParamDisplay, effGetParamName
  kVstMaxProgNameLen   = 24; // used for effGetProgramName, effSetProgramName, effGetProgramNameIndexed
  kVstMaxEffectNameLen = 32; // used for effGetEffectName
  kVstMaxVendorStrLen  = 64; // used for effGetVendorString, amGetVendorString
  kVstMaxProductStrLen = 64; // used for effGetProductString, amGetProductString

// Make a unique ID from 4 charactors
function MakeUniqueID(a,b,c,d:AnsiChar):Int32; inline;
// Cast pointer to IntPtr.
function FromIntPtr(const arg: IntPtr): Pointer; inline;
// Cast IntPtr to pointer.
function ToIntPtr(const ptr: Pointer): IntPtr; inline;
// String copy taking care of null terminator.
function VstStrncpy(Dest: PAnsiChar; Source: PAnsiChar; MaxLen: longword): PAnsiChar;
// String concatenation taking care of null terminator.
function VstStrncat(Dest: PAnsiChar; Source: PAnsiChar; MaxLen: longword): PAnsiChar;

type
  PPERect = ^PERect;
  PERect  = ^TERect;
  // Structure used for effEditGetRect.
  TERect = record
    Top:    int16; // top coordinate, usually 0
    Left:   int16; // left coordinate, usually 0
    Bottom: int16; // bottom coordinate, usually height
    Right:  int16; // right coordinate, usually width
  end;

const
  { String length limits (in characters excl. 0 byte). }

  kVstMaxShortLabelLen = 8;   // used for TVstParameterProperties.shortLabel, TVstPinProperties.shortLabel
  kVstMaxCategLabelLen = 24;  // used for TVstParameterProperties.label
  kVstMaxNameLen       = 64;  // used for TMidiProgramName, TMidiProgramCategory, TMidiKeyName, TVstSpeakerProperties, TVstPinProperties
  kVstMaxLabelLen      = 64;  // used for TVstParameterProperties.label, TVstPinProperties.label
  kVstMaxFileNameLen   = 100; // used for TVstAudioFile.name

type
  // VstEvent Types used by TVstEvent.
  TVstEventTypes = (
    kVstMidiType = 1,  // MIDI event, see TVstMidiEvent
    kVstAudioType,     // deprecated
    kVstVideoType,     // deprecated
    kVstParameterType, // deprecated
    kVstTriggerType,   // deprecated
    kVstSysExType      // MIDI system exclusive, see TVstMidiSysexEvent
  );
  PVstEvent = ^TVstEvent;
  // A generic timestamped event.
  TVstEvent = record
    eType:       TVstEventTypes;    // see TVstEventTypes
    ByteSize:    Int32;             // size of this event, excl. type and byteSize
    DeltaFrames: Int32; // sample frames related to the current block start sample position
    Flags:       Int32;             // generic flags, none defined yet
    Data:        array[0..15] of AnsiChar; // data size may vary, depending on event type
  end;

  PVstEvents = ^TVstEvents;
  // A block of events for the current processed audio block.
  TVstEvents = record
    NumEvents: Int32;  // number of Events in array
    Reserved:  IntPtr; // zero (Reserved for future use)
    Events:    array[0..1] of PVstEvent; // event pointer array, variable size
  end;

  TVstMidiEventFlag  = (
  { means that this event is played life (not in playback from a sequencer track).
    This allows the Plug-In to handle these flagged events with higher priority,
    especially when the Plug-In has a big latency (TAEffect.InitialDelay) }
    kVstMidiEventIsRealtime  // 1<<0
  );
  // Flags used in TVstMidiEvent.
  TVstMidiEventFlags = set of TVstMidiEventFlag;

  PVstMidiEvent = ^TVstMidiEvent;
  // MIDI Event (to be casted from VstEvent).
  TVstMidiEvent = record
    eType:           TVstEventTypes; // kVstMidiType
    ByteSize:        Int32; // sizeof (TVstMidiEvent)
    DeltaFrames:     Int32; // sample frames related to the current block start sample position
    Flags:           TVstMidiEventFlags; // see TVstMidiEventFlags
    NoteLength:      Int32; // (in sample frames) of entire note, if available, else 0
    NoteOffset:      Int32; // offset (in sample frames) into note from note start if available, else 0
    MidiData:        array[0..3] of Byte; // 1 to 3 MIDI bytes; midiData[3] is reserved (zero)
    Detune:          Int8; // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    NoteOffVelocity: Int8; // Note Off Velocity [0, 127]
    Reserved1:       Int8; // zero (Reserved for future use)
    Reserved2:       Int8; // zero (Reserved for future use)
  end;

  PVstMidiSysexEvent = ^TVstMidiSysexEvent;
  // MIDI Sysex Event (to be casted from TVstEvent).
  TVstMidiSysexEvent = record
    eType:       TVstEventTypes; // kVstSysexType
    ByteSize:    Int32;     // sizeof (TVstMidiSysexEvent)
    DeltaFrames: Int32;     // sample frames related to the current block start sample position
    Flags:       Int32;     // none defined yet (should be zero)
    DumpBytes:   Int32;     // byte size of sysexDump
    Resvd1:      IntPtr;    // zero (Reserved for future use)
    SysexDump:   PAnsiChar; // sysex dump
    Resvd2:      IntPtr;    // zero (Reserved for future use)
  end;

  // SMPTE Frame Rates.
  TVstSmpteFrameRate = (
    kVstSmpte24fps = 0,
    kVstSmpte25fps = 1,
    kVstSmpte2997fps = 2,
    kVstSmpte30fps = 3,
    kVstSmpte2997dfps = 4, // 29.97 drop
    kVstSmpte30dfps = 5,   // 30 drop
    kVstSmpteFilm16mm = 6, // Film 16mm
    kVstSmpteFilm35mm = 7, // Film 35mm
    kVstSmpte239fps = 10,  // HDTV: 23.976 fps
    kVstSmpte249fps = 11,  // HDTV: 24.976 fps
    kVstSmpte599fps = 12,  // HDTV: 59.94 fps
    kVstSmpte60fps = 13    // HDTV: 60 fps
  );

  { VstTimeInfo }

  TVstTimeInfoFlag  = (
    kVstTransportChanged,     // 1<<0  indicates that play, cycle or record state has changed
    kVstTransportPlaying,     // 1<<1  set if Host sequencer is currently playing
    kVstTransportCycleActive, // 1<<2  set if Host sequencer is in cycle mode
    kVstTransportRecording,   // 1<<3  set if Host sequencer is in record mode
    kVstAutomationWriting=6,  // 1<<6  set if automation write mode active(record parameter changes)
    kVstAutomationReading,    // 1<<7  set if automation read mode active(play parameter changes)
    kVstNanosValid,           // 1<<8  TVstTimeInfo.NanoSeconds valid
    kVstPpqPosValid,          // 1<<9  TVstTimeInfo.PPQPos valid
    kVstTempoValid,           // 1<<10 TVstTimeInfo.Tempo valid
    kVstBarsValid,            // 1<<11 TVstTimeInfo.BarStartPos valid
    kVstCyclePosValid,        // 1<<12 TVstTimeInfo.CycleStartPos and TVstTimeInfo.CycleEndPos valid
    kVstTimeSigValid,         // 1<<13 TVstTimeInfo.TimeSigNumerator and TVstTimeInfo.TimeSigDenominator valid
    kVstSmpteValid,           // 1<<14 TVstTimeInfo.SmpteOffset and TVstTimeInfo.SmpteFrameRate valid
    kVstClockValid            // 1<<15 TVstTimeInfo.SamplesToNextClock valid
  );
  // Flags used in TVstTimeInfo.
  TVstTimeInfoFlags = set of TVstTimeInfoFlag;

  PVstTimeInfo = ^TVstTimeInfo;
  { TVstTimeInfo requested via amGetTime. }
  {-----------------------------------------------------------------------------
   TVstTimeInfo.SamplePos :
     Current Position. It must always be valid,
     and should not cost a lot to ask for.
     The sample position is ahead of the time displayed to the user.
     In sequencer stop mode, its value does not change.
     A 32 bit integer is too small for sample positions,
     and it's a double to make it easier to convert between ppq and samples.
   TVstTimeInfo.PPQPos :
     At tempo 120, 1 quarter makes 1/2 second,
     so 2.0 ppq translates to 48000 samples at 48kHz sample rate.
     0.25 ppq is one sixteenth note then. if you need something like 480ppq,
     you simply multiply ppq by that scaler.
   TVstTimeInfo.BarStartPos :
     Say we're at bars/beats readout 3.3.3. That's 2 bars + 2 q + 2 sixteenth,
     makes 2 * 4 + 2 + 0.25 = 10.25 ppq. at tempo 120,
     that's 10.25 * 0.5 = 5.125 seconds, times 48000 = 246000 samples
     (if my calculator servers me well :-).
   TVstTimeInfo.SamplesToNextClock :
     MIDI Clock Resolution (24 per Quarter Note),
     can be negative the distance to the next midi clock
     (24 ppq, pulses per quarter) in samples.
     unless samplePos falls precicely on a midi clock,
     this will either be negative such that the previous MIDI clock is addressed,
     or positive when referencing the following (future) MIDI clock.
  -----------------------------------------------------------------------------}
  TVstTimeInfo = record
    SamplePos:      double;    // current Position in audio samples (always valid)
    SampleRate:     double;    // current Sample Rate in Herz (always valid)
    NanoSeconds:    double;    // System Time in nanoseconds (10^-9 second)
    PPQPos:         double;    // Musical Position, in Quarter Note (1.0 equals 1 Quarter Note)
    Tempo:          double;    // current Tempo in BPM (Beats Per Minute)
    BarStartPos:    double;    // last Bar Start Position, in Quarter Note
    CycleStartPos:  double;    // Cycle Start (left locator), in Quarter Note
    CycleEndPos:    double;    // Cycle End (right locator), in Quarter Note
    TimeSigNumerator: Int32;   // Time Signature Numerator (e.g. 3 for 3/4)
    TimeSigDenominator: Int32; // Time Signature Denominator (e.g. 4 for 3/4)
    { SMPTE offset (in SMPTE subframes (bits; 1/80 of a frame)).
      The current SMPTE position can be calculated using SamplePos,
      SampleRate, and SmpteFrameRate.}
    SmpteOffset:    Int32;
    SmpteFrameRate: TVstSmpteFrameRate; // see TVstSmpteFrameRate
    SamplesToNextClock: Int32; // MIDI Clock Resolution (24 Per Quarter Note), can be negative (nearest clock)
    Flags:          TVstTimeInfoFlags;  // see TVstTimeInfoFlags
  end;

  PVstVariableIO = ^TVstVariableIO;
  // Variable IO for Offline Processing.
  TVstVariableIO = record
    Inputs:           PPSingle; // input audio buffers
    Outputs:          PPSingle; // output audio buffers
    NumSamplesInput:  Int32;    // number of incoming samples
    NumSamplesOutput: Int32;    // number of outgoing samples
    NumSamplesInputProcessed: PInt32;  // number of samples actually processed of input
    NumSamplesOutputProcessed: PInt32; // number of samples actually processed of output
  end;

  // Language code returned by amGetLanguage.
  TVstHostLanguage = (
    kVstLangUnknown,   // Unknown
    kVstLangEnglish,   // English
    kVstLangGerman,    // German
    kVstLangFrench,    // French
    kVstLangItalian,   // Italian
    kVstLangSpanish,   // Spanish
    kVstLangJapanese   // Japanese
  );

  // Symbolic precision constants used for effSetProcessPrecision.
  TVstProcessPrecision = (
    kVstProcessPrecision32,  // single precision float (32bits)
    kVstProcessPrecision64   // double precision (64bits)
  );

  TVstParameterFlag  = (
    kVstParameterIsSwitch,                // 1<<0 parameter is a switch (on/off)
    kVstParameterUsesIntegerMinMax,       // 1<<1 minInteger, maxInteger valid
    kVstParameterUsesFloatStep,           // 1<<2 stepFloat, smallStepFloat, largeStepFloat valid
    kVstParameterUsesIntStep,             // 1<<3 stepInteger, largeStepInteger valid
    kVstParameterSupportsDisplayIndex,    // 1<<4 displayIndex valid
    kVstParameterSupportsDisplayCategory, // 1<<5 category, etc. valid
    kVstParameterCanRamp                  // 1<<6 set if parameter value can ramp up/down
  );
  // Flags used in TVstParameterProperties.
  TVstParameterFlags = set of TVstParameterFlag;

  PVstParameterProperties = ^TVstParameterProperties;
  // Parameter Properties used in effGetParameterProperties.
  TVstParameterProperties = record
    StepFloat:      single; // float step
    SmallStepFloat: single; // small float step
    LargeStepFloat: single; // large float step
    sLabel: array[0..63] of AnsiChar;   // parameter label, limit 64
    Flags:          TVstParameterFlags; // see TVstParameterFlags
    MinInteger:     Int32;  // integer minimum
    MaxInteger:     Int32;  // integer maximum
    StepInteger:    Int32;  // integer step
    LargeStepInteger: Int32; // integer step
    ShortLabel: array[0..7] of AnsiChar; // short label, recommended: 6 + delimiter, limit 8

    // The following are for remote controller display purposes.
    // Note that the kVstParameterSupportsDisplayIndex flag must be set.
    // Host can scan all parameters, and find out in what order
    // to display them:

    DisplayIndex: int16; // index where this parameter should be displayed (starting with 0)

    // Host can also possibly display the parameter group (category), such as...
    // ---------------------------
    // Osc 1
    // Wave  Detune  Octave  Mod
    // ---------------------------
    // ...if the plug-in supports it (flag kVstParameterSupportsDisplayCategory)

    Category:      int16; // 0: no category, else group index + 1
    NumParametersInCategory: int16; // number of parameters in category
    Reserved:      int16; // zero
    CategoryLabel: array[0..23] of AnsiChar; // category label, e.g. "Osc 1", limit 24
    Future:        array[0..15] of byte; // reserved for future use
  end;

  // Speaker Arrangement Types
  TVstSpeakerArrangementType = (
    kSpeakerArrUserDefined = -2, // user defined
    kSpeakerArrEmpty = -1,       // empty arrangement
    kSpeakerArrMono = 0,         // M
    kSpeakerArrStereo,           // L R
    kSpeakerArrStereoSurround,   // Ls Rs
    kSpeakerArrStereoCenter,     // Lc Rc
    kSpeakerArrStereoSide,       // Sl Sr
    kSpeakerArrStereoCLfe,       // C Lfe
    kSpeakerArr30Cine,           // L R C
    kSpeakerArr30Music,          // L R S
    kSpeakerArr31Cine,           // L R C Lfe
    kSpeakerArr31Music,          // L R Lfe S
    kSpeakerArr40Cine,           // L R C   S (LCRS)
    kSpeakerArr40Music,          // L R Ls  Rs (Quadro)
    kSpeakerArr41Cine,           // L R C   Lfe S (LCRS+Lfe)
    kSpeakerArr41Music,          // L R Lfe Ls Rs (Quadro+Lfe)
    kSpeakerArr50,               // L R C Ls  Rs
    kSpeakerArr51,               // L R C Lfe Ls Rs
    kSpeakerArr60Cine,           // L R C   Ls  Rs Cs
    kSpeakerArr60Music,          // L R Ls  Rs  Sl Sr
    kSpeakerArr61Cine,           // L R C   Lfe Ls Rs Cs
    kSpeakerArr61Music,          // L R Lfe Ls  Rs Sl Sr
    kSpeakerArr70Cine,           // L R C Ls  Rs Lc Rc
    kSpeakerArr70Music,          // L R C Ls  Rs Sl Sr
    kSpeakerArr71Cine,           // L R C Lfe Ls Rs Lc Rc
    kSpeakerArr71Music,          // L R C Lfe Ls Rs Sl Sr
    kSpeakerArr80Cine,           // L R C Ls  Rs Lc Rc Cs
    kSpeakerArr80Music,          // L R C Ls  Rs Cs Sl Sr
    kSpeakerArr81Cine,           // L R C Lfe Ls Rs Lc Rc Cs
    kSpeakerArr81Music,          // L R C Lfe Ls Rs Cs Sl Sr
    kSpeakerArr102,              // L R C Lfe Ls Rs Tfl Tfc Tfr Trl Trr Lfe2
    kNumSpeakerArr
  );

  TVstPinPropertiesFlag  = (
    kVstPinIsActive,  // 1<<0 pin is active, ignored by Host
    kVstPinIsStereo,  // 1<<1 pin is first of a stereo pair
    kVstPinUseSpeaker // 1<<2 TVstPinProperties.ArrangementType is valid and can be used to get the wanted arrangement
  );
  // Flags used in TVstPinProperties.
  TVstPinPropertiesFlags = set of TVstPinPropertiesFlag;

  PVstPinProperties = ^TVstPinProperties;
  // Pin Properties used in effGetInputProperties and effGetOutputProperties.
  TVstPinProperties = record
    sLabel: array[0..63] of AnsiChar;            // pin name, limit 64
    Flags:           TVstPinPropertiesFlags;     // see TVstPinPropertiesFlags
    ArrangementType: TVstSpeakerArrangementType; // see TVstSpeakerArrangementType
    ShortLabel: array[0..7] of AnsiChar; // short name (recommended: 6 + delimiter), limit 8
    Future:          array[0..47] of byte;       // reserved for future use
  end;

  // Plug-in Categories.
  TVstPlugCategory = (
    kPlugCategUnknown,        // Unknown, category not implemented
    kPlugCategEffect,         // Simple Effect
    kPlugCategSynth,          // VST Instrument (Synths, samplers,...)
    kPlugCategAnalysis,       // Scope, Tuner, ...
    kPlugCategMastering,      // Dynamics, ...
    kPlugCategSpacializer,    // Panners, ...
    kPlugCategRoomFx,         // Delays and Reverbs
    kPlugSurroundFx,          // Dedicated surround processor
    kPlugCategRestoration,    // Denoiser, ...
    kPlugCategOfflineProcess, // Offline Process
    kPlugCategShell, // Plug-in is container of other plug-ins, see effShellGetNextPlugin
    kPlugCategGenerator,      // ToneGenerator, ...
    kPlugCategMaxCount        // Marker to count the categories
  );

  { MIDI Programs }

  TVstMidiProgramNameFlag = (
    kMidiIsOmni // default is multi. for omni mode, channel 0 is used for inquiries and program changes
  );
  // Flags used in TMidiProgramName.
  TVstMidiProgramNameFlags = set of TVstMidiProgramNameFlag;
  PMidiProgramName = ^TMidiProgramName;
  // MIDI Program Description.
  TMidiProgramName = record
    ThisProgramIndex: Int32;    // 0 or greater: fill struct for this program index
    Name: array[0..63] of AnsiChar; // program name, limit 64
    MidiProgram: Int8;          // -1:off, 0-127
    MidiBankMsb: Int8;          // -1:off, 0-127
    MidiBankLsb: Int8;          // -1:off, 0-127
    Reserved:    Int8;          // zero
    ParentCategoryIndex: Int32; // -1:no parent category
    Flags: TVstMidiProgramNameFlags; // omni etc. see TVstMidiProgramNameFlags
  end;

  PMidiProgramCategory = ^TMidiProgramCategory;
  // MIDI Program Category.
  TMidiProgramCategory = record
    ThisCategoryIndex: Int32;   // 0 or greater:  fill struct for this category index.
    Name: array[0..63] of AnsiChar; // name, limit 64
    ParentCategoryIndex: Int32; // -1: no parent category
    Flags: Int32;               // reserved, none defined yet, zero.
  end;

  PMidiKeyName = ^TMidiKeyName;
  // MIDI Key Description.
  TMidiKeyName = record
    ThisProgramIndex: Int32; // 0 or greater:  fill struct for this program index.
    ThisKeyNumber: Int32;    // 0 - 127. fill struct for this key number.
    KeyName: array[0..63] of AnsiChar; // key name, empty means regular key names, limit 64
    Reserved: Int32;         // zero
    Flags: Int32;            // reserved, none defined yet, zero.
  end;

  { Surround Setup }

  // Speaker Types.
  TVstSpeakerType = (
    kSpeakerM,                      // Mono (M)
    kSpeakerL,                      // Left (L)
    kSpeakerR,                      // Right (R)
    kSpeakerC,                      // Center (C)
    kSpeakerLfe,                    // Subbass (Lfe)
    kSpeakerLs,                     // Left Surround (Ls)
    kSpeakerRs,                     // Right Surround (Rs)
    kSpeakerLc,                     // Left of Center (Lc)
    kSpeakerRc,                     // Right of Center (Rc)
    kSpeakerS,                      // Surround (S)
    kSpeakerCs = kSpeakerS,         // Center of Surround (Cs) = Surround (S)
    kSpeakerSl,                     // Side Left (Sl)
    kSpeakerSr,                     // Side Right (Sr)
    kSpeakerTm,                     // Top Middle (Tm)
    kSpeakerTfl,                    // Top Front Left (Tfl)
    kSpeakerTfc,                    // Top Front Center (Tfc)
    kSpeakerTfr,                    // Top Front Right (Tfr)
    kSpeakerTrl,                    // Top Rear Left (Trl)
    kSpeakerTrc,                    // Top Rear Center (Trc)
    kSpeakerTrr,                    // Top Rear Right (Trr)
    kSpeakerLfe2,                   // Subbass 2 (Lfe2)
    kSpeakerUndefined = $7fffffff   // Undefined
  );
  PVstSpeakerProperties = ^TVstSpeakerProperties;
  { Speaker Properties.
  The origin for azimuth is right (as by math conventions dealing with radians).
  The elevation origin is also right, visualizing a rotation of a circle across the
  -pi/pi axis of the horizontal circle. Thus, an elevation of -pi/2 corresponds
  to bottom, and a speaker standing on the left, and 'beaming' upwards would have
  an azimuth of -pi, and an elevation of pi/2.
  For user interface representation, grads are more likely to be used, and the
  origins will obviously 'shift' accordingly. }
  TVstSpeakerProperties = record
    Azimuth:   single; // unit: rad, range: -PI...PI, exception: 10.f for LFE channel
    Elevation: single; // unit: rad, range: -PI/2...PI/2, exception: 10.f for LFE channel
    Radius:    single; // unit: meter, exception: 0.f for LFE channel
    Reserved:  single; // zero (reserved for future use)
    Name: array[0..63] of AnsiChar; // for new setups, new names should be given (L/R/C... won't do), limit 64
    eType:     TVstSpeakerType;      // see TVstSpeakerType
    Future:    array[0..27] of byte; // reserved for future use
  end;

  PPVstSpeakerArrangement = ^PVstSpeakerArrangement;
  PVstSpeakerArrangement  = ^TVstSpeakerArrangement;
  // Speaker Arrangement.
  TVstSpeakerArrangement = record
    eType: TVstSpeakerArrangementType; // e.g. kSpeakerArr51 for 5.1, see TVstSpeakerArrangementType
    NumChannels: Int32; // number of channels in this speaker arrangement
    Speakers: array[0..7] of TVstSpeakerProperties; // variable sized speaker array
  end;

  // User-defined speaker types, to be extended in the negative range.
  // Will be handled as their corresponding speaker types with abs values:
  // e.g abs(ord(kSpeakerU1)) = ord(kSpeakerL), abs(ord(kSpeakerU2)) = ord(kSpeakerR)
  TVstUserSpeakerType = (
    kSpeakerU32 = -32,
    kSpeakerU31,
    kSpeakerU30,
    kSpeakerU29,
    kSpeakerU28,
    kSpeakerU27,
    kSpeakerU26,
    kSpeakerU25,
    kSpeakerU24,
    kSpeakerU23,
    kSpeakerU22,
    kSpeakerU21,
    kSpeakerU20,      // = kSpeakerLfe2
    kSpeakerU19,      // = kSpeakerTrr
    kSpeakerU18,      // = kSpeakerTrc
    kSpeakerU17,      // = kSpeakerTrl
    kSpeakerU16,      // = kSpeakerTfr
    kSpeakerU15,      // = kSpeakerTfc
    kSpeakerU14,      // = kSpeakerTfl
    kSpeakerU13,      // = kSpeakerTm
    kSpeakerU12,      // = kSpeakerSr
    kSpeakerU11,      // = kSpeakerSl
    kSpeakerU10,      // = kSpeakerCs
    kSpeakerU9,       // = kSpeakerS
    kSpeakerU8,       // = kSpeakerRc
    kSpeakerU7,       // = kSpeakerLc
    kSpeakerU6,       // = kSpeakerRs
    kSpeakerU5,       // = kSpeakerLs
    kSpeakerU4,       // = kSpeakerLfe
    kSpeakerU3,       // = kSpeakerC
    kSpeakerU2,       // = kSpeakerR
    kSpeakerU1        // = kSpeakerL
  );

  { Offline Processing }

  TVstOfflineTaskFlag  = (
    kVstOfflineUnvalidParameter, // 1<<0  set by Host
    kVstOfflineNewFile,          // 1<<1  set by Host
    kVstOfflinePlugError=10,     // 1<<10 set by plug-in
    kVstOfflineInterleavedAudio, // 1<<11 set by plug-in
    kVstOfflineTempOutputFile,   // 1<<12 set by plug-in
    kVstOfflineFloatOutputFile,  // 1<<13 set by plug-in
    kVstOfflineRandomWrite,      // 1<<14 set by plug-in
    kVstOfflineStretch,          // 1<<15 set by plug-in
    kVstOfflineNoThread          // 1<<16 set by plug-in
  );
  // Flags used in TVstOfflineTask.
  TVstOfflineTaskFlags = set of TVstOfflineTaskFlag;

  PVstOfflineTask = ^TVstOfflineTask;
  // Offline Task Description.
  TVstOfflineTask = record
    ProcessName: array[0..95] of AnsiChar; // set by plug-in

    // audio access

    ReadPosition:       double;           // set by plug-in/Host
    WritePosition:      double;           // set by plug-in/Host
    ReadCount:          Int32;            // set by plug-in/Host
    WriteCount:         Int32;            // set by plug-in
    SizeInputBuffer:    Int32;            // set by Host
    SizeOutputBuffer:   Int32;            // set by Host
    InputBuffer:        Pointer;          // set by Host
    OutputBuffer:       Pointer;          // set by Host
    PositionToProcessFrom: double;        // set by Host
    NumFramesToProcess: double;           // set by Host
    MaxFramesToWrite:   double;           // set by plug-in

    // other data access

    ExtraBuffer: Pointer;                 // set by plug-in
    Value:       Int32;                   // set by Host or plug-in
    Index:       Int32;                   // set by Host or plug-in

    // file attributes

    NumFramesInSourceFile: double;        // set by Host
    SourceSampleRate: double;             // set by Host or plug-in
    DestinationSampleRate: double;        // set by Host or plug-in
    NumSourceChannels: Int32;             // set by Host or plug-in
    NumDestinationChannels: Int32;        // set by Host or plug-in
    SourceFormat: Int32;                  // set by Host
    DestinationFormat: Int32;             // set by plug-in
    OutputText: array[0..511] of AnsiChar; // set by plug-in or Host

    // progress notification

    Progress:     double;                 // set by plug-in
    ProgressMode: Int32;                  // Reserved for future use
    ProgressText: array[0..99] of AnsiChar; // set by plug-in
    Flags:        TVstOfflineTaskFlags;   // set by Host and plug-in; see TVstOfflineTaskFlags
    ReturnValue:  Int32;                  // Reserved for future use
    HostOwned:    Pointer;                // set by Host
    PlugOwned:    Pointer;                // set by plug-in
    Future:       array[0..1023] of byte; // Reserved for future use
  end;

  // Option passed to amOfflineRead/amOfflineWrite.
  TVstOfflineOption = (
    kVstOfflineAudio,       // reading/writing audio samples
    kVstOfflinePeaks,       // reading graphic representation
    kVstOfflineParameter,   // reading/writing parameters
    kVstOfflineMarker,      // reading/writing marker
    kVstOfflineCursor,      // reading/moving edit cursor
    kVstOfflineSelection,   // reading/changing selection
    kVstOfflineQueryFiles   // to request the Host to call asynchronously effOfflineNotify
  );

  TVstAudioFileFlag  = (
    kVstOfflineReadOnly,            // 1<<0  set by Host (in call effOfflineNotify)
    kVstOfflineNoRateConversion,    // 1<<1  set by Host (in call effOfflineNotify)
    kVstOfflineNoChannelChange,     // 1<<2  set by Host (in call effOfflineNotify)
    kVstOfflineCanProcessSelection=10, // 1<<10  set by plug-in (in call amOfflineStart)
    kVstOfflineNoCrossfade,         // 1<<11  set by plug-in (in call amOfflineStart)
    kVstOfflineWantRead,            // 1<<12  set by plug-in (in call amOfflineStart)
    kVstOfflineWantWrite,           // 1<<13  set by plug-in (in call amOfflineStart)
    kVstOfflineWantWriteMarker,     // 1<<14  set by plug-in (in call amOfflineStart)
    kVstOfflineWantMoveCursor,      // 1<<15  set by plug-in (in call amOfflineStart)
    kVstOfflineWantSelect           // 1<<16  set by plug-in (in call amOfflineStart)
  );
  // Flags used in TVstAudioFile.
  TVstAudioFileFlags = set of TVstAudioFileFlag;

  PVstAudioFile = ^TVstAudioFile;
  // Structure passed to effOfflineNotify and amOfflineStart
  TVstAudioFile = record
    Flags:          TVstAudioFileFlags;   // see TVstAudioFileFlags
    HostOwned:      Pointer;              // any data private to Host
    PlugOwned:      Pointer;              // any data private to plug-in
    Name: array[0..99] of AnsiChar;       // file title, limit 100
    UniqueID:       Int32;   // uniquely identify a file during a session
    SampleRate:     double;               // file sample rate
    NumChannels:    Int32;   // number of channels (1 for mono, 2 for stereo...)
    NumFrames:      double;               // number of frames in the audio file
    Format:         Int32;                // Reserved for future use
    EditCursorPosition: double;           // -1 if no such cursor
    SelectionStart: double;  // frame index of first selected frame, or -1
    SelectionSize:  double;               // number of frames in selection, or 0
    SelectedChannelsMask: Int32;          // 1 bit per channel
    NumMarkers:     Int32;                // number of markers in the file
    TimeRulerUnit:  Int32;                // see doc for possible values
    TimeRulerOffset: double; // offset in time ruler (positive or negative)
    Tempo:          double;               // as BPM (Beats Per Minute)
    TimeSigNumerator: Int32;              // time signature numerator
    TimeSigDenominator: Int32;            // time signature denominator
    TicksPerBlackNote: Int32;             // resolution
    SmpteFrameRate: Int32;                // SMPTE rate (set as in TVstTimeInfo)
    Future:         array[0..63] of byte; // Reserved for future use
  end;

  PVstAudioFileMarker = ^TVstAudioFileMarker;
  // Audio file marker.
  TVstAudioFileMarker = record
    Position: double;               // marker position
    Name: array[0..31] of AnsiChar; // marker name
    iType:    Int32;                // marker type
    ID:       Int32;                // marker identifier
    Reserved: Int32;                // reserved for future use
  end;

  { Others }

  PVstWindow = ^TVstWindow;
  // deprecated Structure used for amOpenWindow and amCloseWindow (deprecated in VST 2.4).
  TVstWindow = record
    Title:      array[0..127] of AnsiChar;
    XPos:       int16;
    YPos:       int16;
    Width:      int16;
    Height:     int16;
    Style:      Int32;
    Parent:     Pointer;
    UserHandle: Pointer;
    WinHandle:  Pointer;
    Future:     array [0..103] of byte;
  end;

  // Platform-independent definition of Virtual Keys (used in TVstKeyCode).
  TVstVirtualKey = (
    VKEY_UNKNOWN = 0,
    VKEY_BACK = 1,
    VKEY_TAB,
    VKEY_CLEAR,
    VKEY_RETURN,
    VKEY_PAUSE,
    VKEY_ESCAPE,
    VKEY_SPACE,
    VKEY_NEXT,
    VKEY_END,
    VKEY_HOME,
    VKEY_LEFT,
    VKEY_UP,
    VKEY_RIGHT,
    VKEY_DOWN,
    VKEY_PAGEUP,
    VKEY_PAGEDOWN,
    VKEY_SELECT,
    VKEY_PRINT,
    VKEY_ENTER,
    VKEY_SNAPSHOT,
    VKEY_INSERT,
    VKEY_DELETE,
    VKEY_HELP,
    VKEY_NUMPAD0,
    VKEY_NUMPAD1,
    VKEY_NUMPAD2,
    VKEY_NUMPAD3,
    VKEY_NUMPAD4,
    VKEY_NUMPAD5,
    VKEY_NUMPAD6,
    VKEY_NUMPAD7,
    VKEY_NUMPAD8,
    VKEY_NUMPAD9,
    VKEY_MULTIPLY,
    VKEY_ADD,
    VKEY_SEPARATOR,
    VKEY_SUBTRACT,
    VKEY_DECIMAL,
    VKEY_DIVIDE,
    VKEY_F1,
    VKEY_F2,
    VKEY_F3,
    VKEY_F4,
    VKEY_F5,
    VKEY_F6,
    VKEY_F7,
    VKEY_F8,
    VKEY_F9,
    VKEY_F10,
    VKEY_F11,
    VKEY_F12,
    VKEY_NUMLOCK,
    VKEY_SCROLL,
    VKEY_SHIFT,
    VKEY_CONTROL,
    VKEY_ALT,
    VKEY_EQUALS
  );

  TVstModifierKey  = (
    MODIFIER_SHIFT,     // 1<<0 Shift
    MODIFIER_ALTERNATE, // 1<<1 Alt
    MODIFIER_COMMAND,   // 1<<2 Control on Mac
    MODIFIER_CONTROL    // 1<<3 Ctrl on PC, Apple on Mac
  );
// effEditKeyUp and effEditKeyDown are using variable Value to pass the param TVstModifierKeys
// Due to the type of variable Value is IntPtr, we must pack set to make it equal IntPtr
{$push}{$ifdef CPUX86}{$PackSet 4}{$else}{$PackSet 8}{$endif}
  // Modifier flags used in TVstKeyCode.
  TVstModifierKeys = set of TVstModifierKey;
{$pop}
  PVstKeyCode = ^TVstKeyCode;
  // Structure used for effEditKeyUp/effEditKeyDown.
  TVstKeyCode = record
    Character: Int32;    // ASCII character
    Virt:      TVstVirtualKey;   // see TVstVirtualKey
    Modifier:  TVstModifierKeys; // see TVstModifierKey
  end;

  PVstFileType = ^TVstFileType;
  { TVstFileType }
  // File filter used in TVstFileSelect.
  TVstFileType = record
    Name:      array[0..127] of AnsiChar; // Display name
    MacType:   array[0..7] of AnsiChar;   // MacOS type
    DosType:   array[0..7] of AnsiChar;   // Windows file extension
    UnixType:  array[0..7] of AnsiChar;   // Unix file extension
    MimeType1: array[0..127] of AnsiChar; // MIME type
    MimeType2: array[0..127] of AnsiChar; // Additional MIME type
    procedure Create(_Name, _MacType, _DosType, _UnixType, _MimeType1, _MimeType2: PAnsiChar);
  end;

  // Command constants used in TVstFileSelect structure.
  TVstFileSelectCommand = (
    kVstFileLoad,          // for loading a file
    kVstFileSave,          // for saving a file
    kVstMultipleFilesLoad, // for loading multiple files
    kVstDirectorySelect    // for selecting a directory/folder
  );
  { Types used in TVstFileSelect structure.}
  TVstFileSelectType = (
    kVstFileType = 0 // regular file selector
  );
  PVstFileSelect = ^TVstFileSelect;
  { File Selector Description used in amOpenFileSelector.}
  TVstFileSelect = record
    Command:        TVstFileSelectCommand;  // see TVstFileSelectCommand
    eType:          TVstFileSelectType;     // see TVstFileSelectType
    MacCreator:     Int32;                  // optional: 0 = no creator
    NumFileTypes:   Int32;                  // number of fileTypes
    FileTypes:      PVstFileType;           // list of fileTypes  see TVstFileType
    Title:          array[0..1023] of AnsiChar; // text to display in file selector's title
    InitialPath:    PAnsiChar; // initial path
    { use with kVstFileLoad and kVstDirectorySelect.
      null: Host allocates memory, plug-in must call amCloseFileSelector! }
    ReturnPath:     PAnsiChar;
    SizeReturnPath: Int32; // size of allocated memory for return paths
    { use with kVstMultipleFilesLoad. Host allocates memory,
      plug-in must call amCloseFileSelector! }
    ReturnMultiplePaths: PPAnsiChar;
    NbReturnPath:   Int32;  // number of selected paths
    Reserved:       IntPtr; // reserved for Host application
    Future:         array[0..115] of byte; // reserved for future use
  end;

  PVstPatchChunkInfo = ^TVstPatchChunkInfo;
  // Structure used for effBeginLoadBank/effBeginLoadProgram.
  TVstPatchChunkInfo = record
    Version:        Int32; // Format Version (should be 1)
    PluginUniqueID: Int32; // UniqueID of the plug-in
    PluginVersion:  Int32; // Plug-in Version
    NumElements:    Int32; // Number of Programs (Bank) or Parameters (Program)
    Future:         array[0..47] of byte; // Reserved for future use
  end;

  // PanLaw Type.
  TVstPanLawType = (
    kLinearPanLaw,      // L = pan * M; R = (1 - pan) * M;
    kEqualPowerPanLaw   // L = pow (pan, 0.5) * M; R = pow ((1 - pan), 0.5) * M;
  );

  // Process Levels returned by amGetCurrentProcessLevel.
  TVstProcessLevels = (
    kVstProcessLevelUnknown,  // not supported by Host
    kVstProcessLevelUser,     // 1: currently in user thread (GUI)
    kVstProcessLevelRealtime, // 2: currently in audio thread (where process is called)
    kVstProcessLevelPrefetch, // 3: currently in 'sequencer' thread (MIDI, timer etc)
    kVstProcessLevelOffline   // 4: currently offline processing and thus in user thread
  );

  // Automation States returned by amGetAutomationState.
  TVstAutomationStates = (
    kVstAutomationUnsupported, // not supported by Host
    kVstAutomationOff,         // off
    kVstAutomationRead,        // read
    kVstAutomationWrite,       // write
    kVstAutomationReadWrite    // read and write
  );

{$pop}

{ FxStore }
const
  CMagic           = 'CcnK';
  FMagic           = 'FxCk';
  BankMagic        = 'FxBk';
  ChunkPresetMagic = 'FPCh';
  ChunkBankMagic   = 'FBCh';
{-------------------------------------------------------------------------------
  Note: The C data structures below are for illustration only.
        You can not read/write them directly.
  The byte order on disk of fxp and fxb files is Big Endian. You have to swap integer
  and floating-point values on Little Endian platforms (Windows, MacIntel)!
-------------------------------------------------------------------------------}
type
  // Program (fxp) structure.
  PFxProgram = ^TFxProgram;
  TFxProgram = record
    ChunkMagic: Int32; // 'CcnK'
    ByteSize:   Int32; // size of this chunk, excl. magic + byteSize
    FxMagic:    Int32; // 'FxCk' (regular) or 'FPCh' (opaque chunk)
    Version:    Int32; // format version (currently 1)
    FxID:       Int32; // fx unique ID
    FxVersion:  Int32; // fx version
    NumParams:  Int32; // number of parameters
    PrgName:    array[0..27] of AnsiChar; // program name (null-terminated ASCII string)
    Content: record                   // program content depending on fxMagic
           case longint of
        0: (Params: array[0..0] of single); // variable sized array with parameter values
        1: (Data: record // program chunk data
            Size: Int32; // size of program data
            Chunk: array[0..0] of AnsiChar; // variable sized array with opaque program data
          end);
    end;
  end;

  // Bank (fxb) structure.
  PFxBank = ^TFxBank;
  TFxBank = record
    ChunkMagic:     Int32; // 'CcnK'
    ByteSize:       Int32; // size of this chunk, excl. magic + byteSize
    FxMagic:        Int32; // 'FxBk' (regular) or 'FBCh' (opaque chunk)
    Version:        Int32; // format version (1 or 2)
    FxID:           Int32; // fx unique ID
    FxVersion:      Int32; // fx version
    NumPrograms:    Int32; // number of programs
    CurrentProgram: Int32; // version 2: current program number
    Future:         array[0..123] of byte; // reserved, should be zero
    Content: record                        // bank content depending on fxMagic
           case longint of
        0: (Programs: array[0..0] of TfxProgram); // variable number of programs
        1: (Data: record // bank chunk data
            Size: Int32; // size of bank data
            Chunk: array[0..0] of AnsiChar; // variable sized array with opaque bank data
          end);
    end;
  end;

implementation

function MakeUniqueID(a, b, c, d: AnsiChar): Int32;
begin
  Result:=ord(a) shl 24 or ord(b) shl 16 or ord(c) shl 8 or ord(d);
end;

function FromIntPtr(const arg: IntPtr): Pointer;
begin
  Result := PPointer(@arg)^;
end;

function ToIntPtr(const ptr: Pointer): IntPtr;
begin
  Result := PIntPtr(@ptr)^;
end;

function VstStrncpy(Dest: PAnsiChar; Source: PAnsiChar; MaxLen: longword): PAnsiChar;
begin
  Move(Source^, Dest^, maxlen);
  Dest[maxlen] := #0;
  Result := Dest;
end;

function VstStrncat(Dest: PAnsiChar; Source: PAnsiChar; MaxLen: longword): PAnsiChar;
var
  Len: longword;
begin
  Len := StrLen(Dest);
  Move(Source^, (Dest + Len)^, maxLen - Len);
  Dest[maxLen] := #0;
  Result := Dest;
end;

{ TVstFileType }

procedure TVstFileType.Create(_Name, _MacType, _DosType, _UnixType, _MimeType1, _MimeType2: PAnsiChar);
begin
  FillChar(self, sizeof(self), 0);
  if Assigned(_Name) then vststrncpy(Name, _Name, 127);
  if Assigned(_MacType) then vststrncpy(MacType, _MacType, 7);
  if Assigned(_DosType) then vststrncpy(DosType, _DosType, 7);
  if Assigned(_UnixType) then vststrncpy(UnixType, _UnixType, 7);
  if Assigned(_MimeType1) then vststrncpy(MimeType1, _MimeType1, 127);
  if Assigned(_MimeType2) then vststrncpy(MimeType2, _MimeType2, 127);
end;

end.

