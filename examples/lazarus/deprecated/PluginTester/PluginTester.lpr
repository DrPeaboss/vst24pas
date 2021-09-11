program PluginTester;

{$WARN 5024 off : Parameter "$1" not used}

uses
 {$ifdef linux}dynlibs, {$endif}
  SysUtils,
  vst2intf;

type
  TVstPluginMain = function(vsthost: THostCallback): PAEffect; cdecl;

  function UniqueIDToString(id: integer): string;
  begin
    Result := '"' + char(id shr 24) + char(id shr 16) + char(id shr 8) + char(id) + '"';
  end;

  function HostCallBack(effect: PAEffect; opcode, index: int32; Value: IntPtr;
    ptr: Pointer; opt: single): IntPtr; cdecl;
  begin
    Result := 0;
    case TAudioMasterOpcodes(opcode) of
      amVersion: Result := 2400;
      amGetBlockSize: Result := 512;
      amGetLanguage: Result := Ord(kVstLangUnknown);
      amGetSampleRate: Result := 44100;
      else;
    end;
  end;

  procedure MyHalt;
  begin
    Writeln('press enter to exit --------');
    Readln;
    halt;
  end;

  function PtrToHex(P:Pointer):string;
  begin
    Result:=IntToHex(ToIntPtr(P));
  end;

  function VDispatcher(effect: PAEffect; opcode: TAEOpcodes; index: int32 = 0;
    Value: IntPtr = 0; ptr: Pointer = nil; opt: single = 0): IntPtr;
  begin
    if Assigned(effect) then
      Result := effect^.Dispatcher(effect, Ord(opcode), index, Value, ptr, opt);
  end;

var
  hDll: TLibHandle;
  VstMain: TVstPluginMain;
  effect: PAEffect;
  flag: TVstAEffectFlag;
  buffer: array[0..255] of char;
  rect: PERect;
  inputs, outputs: PPSingle;
  i: integer;
  sDll: string;
begin
  if ParamCount < 1 then
  begin
    Writeln('Please input a plugin file');
    MyHalt;
  end;

  sDll := ParamStr(1);
  if ExtractFileExt(sDll)='' then sDll:=sDll+'.dll';
  Writeln('Get plugin file: ', sDll);

  if FileExists(sDll) then hDll := LoadLibrary(sDll);
  if hDll = 0 then
  begin
    Writeln('Cannot load library');
    MyHalt;
  end;

  Writeln('Dll: "', sDll, '" is loaded, the handle is ', IntToHex(hDll));

  Pointer(VstMain) := GetProcAddress(hDll, 'VSTPluginMain');
  if Assigned(VstMain) then
    Writeln('VSTPluginMain got, address is ', IntToHex(ToIntPtr(VstMain)))
  else
  begin
    Pointer(VstMain) := GetProcAddress(hDll, 'main');
    if Assigned(VstMain) then
      Writeln('main got, address is ', IntToHex(ToIntPtr(VstMain)))
    else
    begin
      Writeln('Cannot find "VSTPluginMain" or "main"');
      MyHalt;
    end;
  end;

  // Get memory for process test
  inputs := GetMem(2 * sizeof(pointer));
  outputs := GetMem(2 * sizeof(pointer));
  inputs[0] := GetMem(10 * sizeof(single));
  inputs[1] := GetMem(10 * sizeof(single));
  outputs[0] := GetMem(10 * sizeof(single));
  outputs[1] := GetMem(10 * sizeof(single));

  for i := 0 to 9 do
  begin
    inputs[0][i] := -0.80;
    inputs[1][i] := 0.80;
    //outputs[0][i] := -0.80;
    //outputs[1][i] := 0.80;
  end;

  Writeln('--------- Start open the plugin ------------');
  try
    effect := VstMain(@HostCallBack);
    Writeln('The address of effect is ', PtrToHex(effect));
    Writeln('Magic is ', IntToHex(effect^.Magic), ' or ', UniqueIDToString(effect^.Magic));
    Writeln('Dispatcher address is ', PtrToHex(effect^.Dispatcher));
    Writeln('GetParameter address is ', PtrToHex(effect^.GetParameter));
    Writeln('SetParameter address is ', PtrToHex(effect^.SetParameter));
    Writeln('Process address is ', PtrToHex(effect^.Process));
    Writeln('ProcessReplacing address is ', PtrToHex(effect^.ProcessReplacing));
    Writeln('ProcessDoubleReplacing address is ', PtrToHex(effect^.ProcessDoubleReplacing));
    Writeln('Object address is ', PtrToHex(effect^.Obj));
    Writeln('User address is ', PtrToHex(effect^.User));
    Writeln('Number of programs is ', effect^.NumPrograms);
    Writeln('Number of params is ', effect^.NumParams);
    Writeln('Number of inputs is ', effect^.NumInputs);
    Writeln('Number of outputs is ', effect^.NumOutputs);
    Writeln('Unique ID is ', IntToHex(effect^.UniqueID), ' or ', UniqueIDToString(effect^.UniqueID));
    Writeln('Version is ', effect^.Version);
    Write('Flags are: ');
    for flag in effect^.Flags do
      Write(flag, ' ');
    Writeln;

    Writeln('---------------- Start open plugin -------------------');
    VDispatcher(effect, effopen);
    VDispatcher(effect, effSetSampleRate, 0, 0, nil, 44100);
    VDispatcher(effect,effSetBlockSize,0,512);
    VDispatcher(effect, effMainsChanged, 0, 1);
    Writeln('Vst version is ', VDispatcher(effect, effGetVstVersion));
    VDispatcher(effect, effGetEffectName, 0, 0, @buffer, 0);
    Writeln('Effect name is ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetVendorString, 0, 0, @buffer, 0);
    Writeln('Vendor string is ', buffer);
    buffer[0] := #0;
    Writeln('Vendor version is ', VDispatcher(effect, effGetVendorVersion, 0, 0, nil, 0));
    VDispatcher(effect, effGetProductString, 0, 0, @buffer, 0);
    Writeln('Product string is ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetParamName, 0, 0, @buffer, 0);
    Writeln('Parameter 0 name is ', buffer);
    buffer[0] := #0;
    Writeln('Parameter 0 default is ', effect^.GetParameter(effect, 0): 7: 7);
    VDispatcher(effect, effGetParamLabel, 0, 0, @buffer, 0);
    Writeln('Parameter 0 label is ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetParamDisplay, 0, 0, @buffer, 0);
    Writeln('Parameter 0 display is ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetProgramName, 0, 0, @buffer, 0);
    Writeln('Current program name is ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effEditGetRect, 0, 0, @rect, 0);
    if Assigned(rect) then
      Writeln('Rect L T R B is ', rect^.Left, ' ', rect^.Top, ' ', rect^.Right, ' ', rect^.Bottom);

    Writeln('---------- Process test ----------');
    // Process test
    effect^.SetParameter(effect, 0, 0.1);
    VDispatcher(effect, effGetParamName, 0, 0, @buffer, 0);
    Write('Parameter 0: ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetParamDisplay, 0, 0, @buffer, 0);
    Write(' ', buffer);
    buffer[0] := #0;
    VDispatcher(effect, effGetParamLabel, 0, 0, @buffer, 0);
    Writeln(' ', buffer, '  value now: ',0.1: 3: 3);
    buffer[0] := #0;

    VDispatcher(effect, effStartProcess, 0, 0, nil, 0);
    Writeln('Processing ...');
    effect^.ProcessReplacing(effect, inputs, outputs, 10);
    VDispatcher(effect, effStopProcess, 0, 0, nil, 0);
    // Display values
    for i := 0 to 9 do
    begin
      Write(i, ' | inputs', ': L=', inputs[0][i]: 3: 3, ' R=', inputs[1][i]: 3: 3);
      Writeln(' | outputs', ': L=', outputs[0][i]: 3: 3, ' R=', outputs[1][i]: 3: 3);
    end;

    VDispatcher(effect, effClose, 0, 0, nil, 0);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  //Writeln('----- Press enter to free library and memory ...');
  //Readln;
  FreeLibrary(hDll);
  Freemem(inputs[0]);
  Freemem(inputs[1]);
  Freemem(outputs[0]);
  Freemem(outputs[1]);
  Freemem(inputs);
  Freemem(outputs);
  Writeln('-------------END----------------');
  Readln;
end.
