{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2utils
// Description : Utility functions
// Created by  : PeaZomboss, 2021/09

// Some utility functions for vst2intf
-------------------------------------------------------------------------------}

unit vst2utils;

{$I vst2def.inc}

interface

uses
  vst2intf;


// Convert TAEffectOpcodes to strings
function VstAEOpcode2Str(opcode:TAEffectOpcodes):string;overload;
function VstAEOpcode2Str(opcode:Int32):string;overload;
// Convert TAudioMasterOpcodes to strings
function VstAMOpcode2Str(opcode:TAudioMasterOpcodes):string;overload;
function VstAMOpcode2Str(opcode:Int32):string;overload;
// Convert the amplitude to decibels
function VstAmp2dB(const value: double): double; inline; overload;
function VstAmp2dB(const value: single): single; inline; overload;
// Convert the decibels to amplitude
function VstdB2Amp(const value: double): double; inline; overload;
function VstdB2Amp(const value: single): single; inline; overload;
// Convert the float number to string with length 8
function VstFloat2String(const value: single): AnsiString;
// Convert the float number to dB string with length 8
function VstAmp2dBString(const value: single): AnsiString;
// Convert the integer part of float number to string with length 8
function VstInt2String(const value: single): AnsiString;
// Convert strings to THostCanDo
function VstString2HostCanDo(const str:ansistring):THostCanDo;overload;
function VstString2HostCanDo(sz:PAnsiChar):THostCanDo;overload;
// Convert THostCanDo to strings
function VstHostCanDo2String(hcd:THostCanDo):ansistring;
// Convert strings to TPlugCanDo
function VstString2PlugCanDo(const str:ansistring):TPlugCanDo;overload;
function VstString2PlugCanDo(sz:PAnsiChar):TPlugCanDo;overload;
// Convert TPlugCanDo to strings
function VstPlugCanDo2String(pcd:TPlugCanDo):ansistring;


implementation

uses
  sysutils,math;

function VstAEOpcode2Str(opcode:TAEffectOpcodes):string;
begin
  case opcode of
    effOpen:Result:='effOpen';
    effClose:Result:='effClose';
    effSetProgram:Result:='effSetProgram';
    effGetProgram:Result:='effGetProgram';
    effSetProgramName:Result:='effSetProgramName';
    effGetProgramName:Result:='effGetProgramName';
    effGetParamLabel:Result:='effGetParamLabel';
    effGetParamDisplay:Result:='effGetParamDisplay';
    effGetParamName:Result:='effGetParamName';
    effGetVu:Result:='effGetVu';
    effSetSampleRate:Result:='effSetSampleRate';
    effSetBlockSize:Result:='effSetBlockSize';
    effMainsChanged:Result:='effMainsChanged';
    effEditGetRect:Result:='effEditGetRect';
    effEditOpen:Result:='effEditOpen';
    effEditClose:Result:='effEditClose';
    effEditDraw:Result:='effEditDraw';
    effEditMouse:Result:='effEditMouse';
    effEditKey:Result:='effEditKey';
    effEditIdle:Result:='effEditIdle';
    effEditTop:Result:='effEditTop';
    effEditSleep:Result:='effEditSleep';
    effIdentify:Result:='effIdentify';
    effGetChunk:Result:='effGetChunk';
    effSetChunk:Result:='effSetChunk';
    effProcessEvents:Result:='effProcessEvents';
    effCanBeAutomated:Result:='effCanBeAutomated';
    effString2Parameter:Result:='effString2Parameter';
    effGetNumProgramCategories:Result:='effGetNumProgramCategories';
    effGetProgramNameIndexed:Result:='effGetProgramNameIndexed';
    effCopyProgram:Result:='effCopyProgram';
    effConnectInput:Result:='effConnectInput';
    effConnectOutput:Result:='effConnectOutput';
    effGetInputProperties:Result:='effGetInputProperties';
    effGetOutputProperties:Result:='effGetOutputProperties';
    effGetPlugCategory:Result:='effGetPlugCategory';
    effGetCurrentPosition:Result:='effGetCurrentPosition';
    effGetDestinationBuffer:Result:='effGetDestinationBuffer';
    effOfflineNotify:Result:='effOfflineNotify';
    effOfflinePrepare:Result:='effOfflinePrepare';
    effOfflineRun:Result:='effOfflineRun';
    effProcessVarIO:Result:='effProcessVarIO';
    effSetSpeakerArrangement:Result:='effSetSpeakerArrangement';
    effSetBlockSizeAndSampleRate:Result:='effSetBlockSizeAndSampleRate';
    effSetBypass:Result:='effSetBypass';
    effGetEffectName:Result:='effGetEffectName';
    effGetErrorText:Result:='effGetErrorText';
    effGetVendorString:Result:='effGetVendorString';
    effGetProductString:Result:='effGetProductString';
    effGetVendorVersion:Result:='effGetVendorVersion';
    effVendorSpecific:Result:='effVendorSpecific';
    effCanDo:Result:='effCanDo';
    effGetTailSize:Result:='effGetTailSize';
    effIdle:Result:='effIdle';
    effGetIcon:Result:='effGetIcon';
    effSetViewPosition:Result:='effSetViewPosition';
    effGetParameterProperties:Result:='effGetParameterProperties';
    effKeysRequired:Result:='effKeysRequired';
    effGetVstVersion:Result:='effGetVstVersion';
{$ifdef VST_2_1_EXTENSIONS}
    effEditKeyDown:Result:='effEditKeyDown';
    effEditKeyUp:Result:='effEditKeyUp';
    effSetEditKnobMode:Result:='effSetEditKnobMode';
    effGetMidiProgramName:Result:='effGetMidiProgramName';
    effGetCurrentMidiProgram:Result:='effGetCurrentMidiProgram';
    effGetMidiProgramCategory:Result:='effGetMidiProgramCategory';
    effHasMidiProgramsChanged:Result:='effHasMidiProgramsChanged';
    effGetMidiKeyName:Result:='effGetMidiKeyName';
    effBeginSetProgram:Result:='effBeginSetProgram';
    effEndSetProgram:Result:='effEndSetProgram';
{$endif}
{$ifdef VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement:Result:='effGetSpeakerArrangement';
    effShellGetNextPlugin:Result:='effShellGetNextPlugin';
    effStartProcess:Result:='effStartProcess';
    effStopProcess:Result:='effStopProcess';
    effSetTotalSampleToProcess:Result:='effSetTotalSampleToProcess';
    effSetPanLaw:Result:='effSetPanLaw';
    effBeginLoadBank:Result:='effBeginLoadBank';
    effBeginLoadProgram:Result:='effBeginLoadProgram';
{$endif}
{$ifdef VST_2_4_EXTENSIONS}
    effSetProcessPrecision:Result:='effSetProcessPrecision';
    effGetNumMidiInputChannels:Result:='effGetNumMidiInputChannels';
    effGetNumMidiOutputChannels:Result:='effGetNumMidiOutputChannels';
{$endif}
  end;
end;

function VstAEOpcode2Str(opcode:Int32):string;
begin
  if (opcode>=0) and (opcode<kVstAEOpcodeMax) then
    Result:=VstAEOpcode2Str(TAEOpcodes(opcode))
  else
    Result:='Unknown opcode '+IntToStr(opcode);
end;

function VstAMOpcode2Str(opcode:TAudioMasterOpcodes):string;
begin
  case opcode of
    amAutomate:Result:='amAutomate';
    amVersion:Result:='amVersion';
    amCurrentId:Result:='amCurrentId';
    amIdle:Result:='amIdle';
    amPinConnected:Result:='amPinConnected';
    amUnused:Result:='';
    amWantMidi:Result:='amWantMidi';
    amGetTime:Result:='amGetTime';
    amProcessEvents:Result:='amProcessEvents';
    amSetTime:Result:='amSetTime';
    amTempoAt:Result:='amTempoAt';
    amGetNumAutomatableParameters:Result:='amGetNumAutomatableParameters';
    amGetParameterQuantization:Result:='amGetParameterQuantization';
    amIOChanged:Result:='amIOChanged';
    amNeedIdle:Result:='amNeedIdle';
    amSizeWindow:Result:='amSizeWindow';
    amGetSampleRate:Result:='amGetSampleRate';
    amGetBlockSize:Result:='amGetBlockSize';
    amGetInputLatency:Result:='amGetInputLatency';
    amGetOutputLatency:Result:='amGetOutputLatency';
    amGetPreviousPlug:Result:='amGetPreviousPlug';
    amGetNextPlug:Result:='amGetNextPlug';
    amWillReplaceOrAccumulate:Result:='amWillReplaceOrAccumulate';
    amGetCurrentProcessLevel:Result:='amGetCurrentProcessLevel';
    amGetAutomationState:Result:='amGetAutomationState';
    amOfflineStart:Result:='amOfflineStart';
    amOfflineRead:Result:='amOfflineRead';
    amOfflineWrite:Result:='amOfflineWrite';
    amOfflineGetCurrentPass:Result:='amOfflineGetCurrentPass';
    amOfflineGetCurrentMetaPass:Result:='amOfflineGetCurrentMetaPass';
    amSetOutputSampleRate:Result:='amSetOutputSampleRate';
    amGetOutputSpeakerArrangement:Result:='amGetOutputSpeakerArrangement';
    amGetVendorString:Result:='amGetVendorString';
    amGetProductString:Result:='amGetProductString';
    amGetVendorVersion:Result:='amGetVendorVersion';
    amVendorSpecific:Result:='amVendorSpecific';
    amSetIcon:Result:='amSetIcon';
    amCanDo:Result:='amCanDo';
    amGetLanguage:Result:='amGetLanguage';
    amOpenWindow:Result:='amOpenWindow';
    amCloseWindow:Result:='amCloseWindow';
    amGetDirectory:Result:='amGetDirectory';
    amUpdateDisplay:Result:='amUpdateDisplay';
    amBeginEdit:Result:='amBeginEdit';
    amEndEdit:Result:='amEndEdit';
    amOpenFileSelector:Result:='amOpenFileSelector';
    amCloseFileSelector:Result:='amCloseFileSelector';
    amEditFile:Result:='amEditFile';
    amGetChunkFile:Result:='amGetChunkFile';
    amGetInputSpeakerArrangement:Result:='amGetInputSpeakerArrangement';
  end;
end;

function VstAMOpcode2Str(opcode:Int32):string;
begin
  if (opcode>=0) and (opcode<kVstAMOpcodeMax) then
    Result:=VstAMOpcode2Str(TAMOpcodes(opcode))
  else
    Result:='Unknown opcode '+IntToStr(opcode);
end;

function VstAmp2dB(const value: double): double;
begin
  if value>=1E-50 then
    Result:=20*Log10(value)
  else
    Result:=NegInfinity;
end;

function VstAmp2dB(const value:single):single;
begin
  if value>=1E-25 then
    Result:=20*Log10(value)
  else
    Result:=NegInfinity;
end;

function VstdB2Amp(const value: double): double;
begin
  if value >= -1000 then
    Result:=exp(value*0.11512925464970228420)
  else
    Result:=0;
end;

function VstdB2Amp(const value:single):single;
begin
  if value >= -500 then
    Result:=exp(value*0.11512925464970228420)
  else
    Result:=0;
end;

function VstFloat2String(const value:single):AnsiString;
var
  i: integer;
  mantissa: single;
begin
  if value=NegInfinity then Exit('-Inf'); // Cooperate with VstAmp2dB
  if (Value > 999999) or (Value < -99999) then
    Exit('Huge !!');
{$ifdef UNICODE}
  Result := AnsiString(IntToStr(Trunc(Value)));
{$else}
  Result := IntToStr(Trunc(Value));
{$endif}
  mantissa := Abs(Frac(Value));
  if Length(Result) = 6 then
  begin
    // If the param type is *single*, 999998.47, 999998.48 and 999998.49
    // will be considered as 999998.5 due to precision, so the final result
    // will be 999999 rather 999998
    // Actually, 999998.47 to 999998.53 will all be 999998.5
    if mantissa>=0.5 then Result:=VstFloat2String(Int(Value)+1);
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
  mantissa := Frac(mantissa);
  if mantissa >= 0.5 then // Similar reason, see above
  begin
    while Result[i] = '9' do
    begin
      Result[i] := '0';
      Dec(i);
    end;
    if Result[i] <> '.' then
      Inc(Result[i])
    else
      Result := VstFloat2String(Int(Value)+1);
  end;
end;

function VstAmp2dBString(const value:single):AnsiString;
begin
  Result:=VstFloat2String(VstAmp2dB(value));
end;

function VstInt2String(const value:single):AnsiString;
begin
  if (value>9999999) or (value<-999999) then
    Exit('Huge !!');
{$ifdef UNICODE}
  Result:=AnsiString(IntToStr(Trunc(value)));
{$else}
  Result:=IntToStr(Trunc(value));
{$endif}
end;

function VstString2HostCanDo(const str:ansistring):THostCanDo;
begin
  if str=THcdStrings.cdSendVstEvents                then Result:=hcdSendVstEvents
  else if str=THcdStrings.cdSendVstMidiEvent        then Result:=hcdSendVstMidiEvent
  else if str=THcdStrings.cdSendVstTimeInfo         then Result:=hcdSendVstTimeInfo
  else if str=THcdStrings.cdReceiveVstEvents        then Result:=hcdReceiveVstEvents
  else if str=THcdStrings.cdReceiveVstMidiEvent     then Result:=hcdReceiveVstMidiEvent
  else if str=THcdStrings.cdReportConnectionChanges then Result:=hcdReportConnectionChanges
  else if str=THcdStrings.cdAcceptIOChanges         then Result:=hcdAcceptIOChanges
  else if str=THcdStrings.cdSizeWindow              then Result:=hcdSizeWindow
  else if str=THcdStrings.cdOffline                 then Result:=hcdOffline
  else if str=THcdStrings.cdOpenFileSelector        then Result:=hcdOpenFileSelector
  else if str=THcdStrings.cdCloseFileSelector       then Result:=hcdCloseFileSelector
  else if str=THcdStrings.cdStartStopProcess        then Result:=hcdStartStopProcess
  else if str=THcdStrings.cdShellCategory           then Result:=hcdShellCategory
  else if str=THcdStrings.cdSendVstMidiEventFlagIsRealtime then Result:=hcdSendVstMidiEventFlagIsRealtime
  else Result:=hcdUnknown;
end;

function VstString2HostCanDo(sz:PAnsiChar):THostCanDo;
begin
  Result:=VstString2HostCanDo(sz);
end;

function VstHostCanDo2String(hcd:THostCanDo):ansistring;
begin
  case hcd of
    hcdUnknown:                 Result:='';
    hcdSendVstEvents:           Result:=THcdStrings.cdSendVstEvents;
    hcdSendVstMidiEvent:        Result:=THcdStrings.cdSendVstMidiEvent;
    hcdSendVstTimeInfo:         Result:=THcdStrings.cdSendVstTimeInfo;
    hcdReceiveVstEvents:        Result:=THcdStrings.cdReceiveVstEvents;
    hcdReceiveVstMidiEvent:     Result:=THcdStrings.cdReceiveVstMidiEvent;
    hcdReportConnectionChanges: Result:=THcdStrings.cdReportConnectionChanges;
    hcdAcceptIOChanges:         Result:=THcdStrings.cdAcceptIOChanges;
    hcdSizeWindow:              Result:=THcdStrings.cdSizeWindow;
    hcdOffline:                 Result:=THcdStrings.cdOffline;
    hcdOpenFileSelector:        Result:=THcdStrings.cdOpenFileSelector;
    hcdCloseFileSelector:       Result:=THcdStrings.cdCloseFileSelector;
    hcdStartStopProcess:        Result:=THcdStrings.cdStartStopProcess;
    hcdShellCategory:           Result:=THcdStrings.cdShellCategory;
    hcdSendVstMidiEventFlagIsRealtime: Result:=THcdStrings.cdSendVstMidiEventFlagIsRealtime;
  end;
end;

function VstString2PlugCanDo(const str:ansistring):TPlugCanDo;
begin
  if str=TPcdStrings.cdSendVstEvents            then Result:=pcdSendVstEvents
  else if str=TPcdStrings.cdSendVstMidiEvent    then Result:=pcdSendVstEvents
  else if str=TPcdStrings.cdReceiveVstEvents    then Result:=pcdReceiveVstEvents
  else if str=TPcdStrings.cdReceiveVstMidiEvent then Result:=pcdReceiveVstMidiEvent
  else if str=TPcdStrings.cdReceiveVstTimeInfo  then Result:=pcdReceiveVstTimeInfo
  else if str=TPcdStrings.cdOffline             then Result:=pcdOffline
  else if str=TPcdStrings.cdBypass              then Result:=pcdBypass
  else if str=TPcdStrings.cdMidiProgramNames    then Result:=pcdMidiProgramNames
  else Result:=pcdUnknown;
end;

function VstString2PlugCanDo(sz:PAnsiChar):TPlugCanDo;
var
  s:ansistring;
begin
  s:=sz;
  Result:=VstString2PlugCanDo(s);
end;

function VstPlugCanDo2String(pcd:TPlugCanDo):ansistring;
begin
  case pcd of
    pcdUnknown:             Result:='';
    pcdSendVstEvents:       Result:=TPcdStrings.cdSendVstEvents;
    pcdSendVstMidiEvent:    Result:=TPcdStrings.cdSendVstMidiEvent;
    pcdReceiveVstEvents:    Result:=TPcdStrings.cdReceiveVstEvents;
    pcdReceiveVstMidiEvent: Result:=TPcdStrings.cdReceiveVstMidiEvent;
    pcdReceiveVstTimeInfo:  Result:=TPcdStrings.cdReceiveVstTimeInfo;
    pcdOffline:             Result:=TPcdStrings.cdOffline;
    pcdBypass:              Result:=TPcdStrings.cdBypass;
    pcdMidiProgramNames:    Result:=TPcdStrings.cdMidiProgramNames;
  end;
end;


end.

