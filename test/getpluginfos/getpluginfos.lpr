program getpluginfos;

uses
  SysUtils,
  vst2interfaces;

const
  DllName = '..\version_0_2_0_test\v020test.dll';
  //DllName = '..\..\examples\lazarus\bin\example5.dll';
  //DllName64 = '..\..\examples\lazarus\bin\example5_x64.dll';
  DllName64 = '..\version_0_2_0_test\v020test_x64.dll';

type
  TVstPluginMain = function(vsthost: TVstHostCallback): PAEffect; cdecl;

  function UniqueIDToString(id: integer): string;
  begin
    Result := '"' + char(id shr 24) + char(id shr 16) + char(id shr 8) + char(id) + '"';
  end;

  function HostCallBack(effect: PAEffect; opcode: TAudioMasterOpcodes; index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
  begin
    Result := 0;
    case opcode of
      amVersion: Result := 2400;
      amGetBlockSize: Result:=512;
      amGetLanguage: Result:=ord(kVstLangUnknown);
      else;
    end;
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
{$ifdef CPUX86}
  sDll := DllName;
{$else}
  sDll := DllName64;
{$endif}
  hDll := LoadLibrary(sDll);
  Writeln('Dll: "', sDll, '" is loaded, the handle is ', hDll);
  if hDll = 0 then
  begin
    Writeln('Cannot load library');
    Writeln('press enter to exit --------');
    Readln;
    halt;
  end;
  Pointer(VstMain) := GetProcAddress(hDll, 'VSTPluginMain');
  if Assigned(VstMain) then
    Writeln('VSTPluginMain got, address is ', IntToHex(PInteger(@Pointer(VstMain))^))
  else
  begin
    Pointer(VstMain) := GetProcAddress(hDll, 'main');
    if Assigned(VstMain) then
      Writeln('main got, address is ', IntToHex(PInteger(@Pointer(VstMain))^))
    else
    begin
      Writeln('Cannot find "VSTPluginMain" or "main"');
      Writeln('press enter to exit --------');
      Readln;
      halt;
    end;
  end;

  effect := VstMain(@HostCallBack);

  Writeln('Sizeof effect is ', sizeof(effect^));
  Writeln('Magic is ', IntToHex(effect^.Magic), ' or ', UniqueIDToString(effect^.Magic));
  Writeln('Dispatcher address is ', IntToHex(PInteger(@Pointer(effect^.Dispatcher))^));
  Writeln('GetParameter address is ', IntToHex(PInteger(@Pointer(effect^.GetParameter))^));
  Writeln('SetParameter address is ', IntToHex(PInteger(@Pointer(effect^.SetParameter))^));
  Writeln('Process address is ', IntToHex(PInteger(@Pointer(effect^.Process))^));
  Writeln('ProcessReplacing address is ', IntToHex(PInteger(@Pointer(effect^.ProcessReplacing))^));
  Writeln('ProcessDoubleReplacing address is ', IntToHex(PInteger(@Pointer(effect^.ProcessDoubleReplacing))^));
  Writeln('Object address is ', IntToHex(PInteger(@Pointer(effect^.pObject))^));
  Writeln('User address is ', IntToHex(PInteger(@Pointer(effect^.User))^));
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
  try
    Writeln('---------------- Start callback -------------------');
    effect^.Dispatcher(effect, effMainsChanged, 0, 1, nil, 0);
    effect^.Dispatcher(effect, effSetSampleRate, 0, 0, nil, 44100);
    effect^.Dispatcher(effect, effSetBlockSize, 0, 512, nil, 0);
    Writeln('Parameter 0 is ', effect^.GetParameter(effect, 0): 7: 7);
    Writeln('Vst version is ', effect^.Dispatcher(effect, effGetVstVersion, 0, 0, nil, 0));
    effect^.Dispatcher(effect, effGetEffectName, 0, 0, @buffer, 0);
    Writeln('Effect name is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetVendorString, 0, 0, @buffer, 0);
    Writeln('Vendor string is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetProductString, 0, 0, @buffer, 0);
    Writeln('Product string is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetParamName, 0, 0, @buffer, 0);
    Writeln('Parameter 0 name is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetParamLabel, 0, 0, @buffer, 0);
    Writeln('Parameter 0 label is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetParamDisplay, 0, 0, @buffer, 0);
    Writeln('Parameter 0 display is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, effGetProgramName, 0, 0, @buffer, 0);
    Writeln('Current program name is ', buffer);
    buffer[0] := #0;
    Writeln('Vendor version is ', effect^.Dispatcher(effect, effGetVendorVersion, 0, 0, nil, 0));
    effect^.Dispatcher(effect, effEditGetRect, 0, 0, @rect, 0);
    if Assigned(rect) then
      Writeln('Rect L T R B is ', rect^.Left, ' ', rect^.Top, ' ', rect^.Right, ' ', rect^.Bottom);

    inputs  := GetMem(2 * sizeof(pointer));
    outputs := GetMem(2 * sizeof(pointer));
    inputs[0] := GetMem(20 * sizeof(single));
    inputs[1] := GetMem(20 * sizeof(single));
    outputs[0] := GetMem(20 * sizeof(single));
    outputs[1] := GetMem(20 * sizeof(single));
    for i := 0 to 19 do
    begin
      inputs[0][i]  := -0.80;
      inputs[1][i]  := 0.80;
      outputs[0][i] := -0.80;
      outputs[1][i] := 0.80;
    end;
    effect^.SetParameter(effect, 0, 0.1);
    effect^.Dispatcher(effect, effGetParamDisplay, 0, 0, @buffer, 0);
    Writeln('Parameter 0 display is ', buffer);
    buffer[0] := #0;
    //effect^.Dispatcher(effect, effStartProcess, 0, 0, nil, 0);
    effect^.ProcessReplacing(effect, inputs, outputs, 10);
    //effect^.Dispatcher(effect, effStopProcess, 0, 0, nil, 0);
    for i := 0 to 19 do
    begin
      Writeln(i, ': left channel: ', outputs[0][i]: 7: 7, ' right channel: ', outputs[1][i]: 7: 7);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Freemem(inputs[0]);
  Freemem(inputs[1]);
  Freemem(outputs[0]);
  Freemem(outputs[1]);
  FreeLibrary(hDll);
  Writeln('-------------END----------------');
  Readln;
end.
