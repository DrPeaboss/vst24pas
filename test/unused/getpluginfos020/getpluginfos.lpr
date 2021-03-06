program getpluginfos;

uses
  {$ifdef linux}dynlibs,{$endif}
  SysUtils,
  vst2intf;

const
  //DllName = '..\version_0_2_0_test\v020test.dll';
  //DllName = '..\..\examples\lazarus\bin\example5.dll';
  DllName = '..\version_0_2_x_tests\v02xtest2.dll';
  //DllName64 = '..\..\examples\lazarus\bin\example5_x64.dll';
  DllName64 = '..\version_0_2_0_test\v020test_x64.dll';

type
  TVstPluginMain = function(vsthost: THostCallback): PAEffect; cdecl;

  function UniqueIDToString(id: integer): string;
  begin
    Result := '"' + char(id shr 24) + char(id shr 16) + char(id shr 8) + char(id) + '"';
  end;

  function HostCallBack(effect: PAEffect; opcode, index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;
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
  if ParamCount > 0 then
  begin
    sDll := ParamStr(1);
    Writeln('Get param: ', sDll);
  end else
  begin
{$ifdef CPUX86}
    sDll := DllName;
{$else}
    sDll := DllName64;
{$endif}
  end;
  hDll := -1;
  if FileExists(sDll) then hDll := LoadLibrary(sDll);
  if hDll = -1 then
  begin
    Writeln('Cannot find this file');
    Writeln('press enter to exit --------');
    Readln;
    halt;
  end;
  if hDll = 0 then
  begin
    Writeln('Cannot load library');
    Writeln('press enter to exit --------');
    Readln;
    halt;
  end;
  Writeln('Dll: "', sDll, '" is loaded, the handle is ', hDll);
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
      Writeln('press enter to exit --------');
      Readln;
      halt;
    end;
  end;
  // Get memory for process test
  inputs  := GetMem(2 * sizeof(pointer));
  outputs := GetMem(2 * sizeof(pointer));
  inputs[0] := GetMem(10 * sizeof(single));
  inputs[1] := GetMem(10 * sizeof(single));
  outputs[0] := GetMem(10 * sizeof(single));
  outputs[1] := GetMem(10 * sizeof(single));
  for i := 0 to 9 do
  begin
    inputs[0][i]  := -0.80;
    inputs[1][i]  := 0.80;
    outputs[0][i] := -0.80;
    outputs[1][i] := 0.80;
  end;
  Writeln('--------- Start open the plugin ------------');
  try
    effect := VstMain(@HostCallBack);
    Writeln('The address of effect is ', IntToHex(ToIntPtr(effect)));
    Writeln('Magic is ', IntToHex(effect^.Magic), ' or ', UniqueIDToString(effect^.Magic));
    Writeln('Dispatcher address is ', IntToHex(ToIntPtr(effect^.Dispatcher)));
    Writeln('GetParameter address is ', IntToHex(ToIntPtr(effect^.GetParameter)));
    Writeln('SetParameter address is ', IntToHex(ToIntPtr(effect^.SetParameter)));
    Writeln('Process address is ', IntToHex(ToIntPtr(effect^.Process)));
    Writeln('ProcessReplacing address is ', IntToHex(ToIntPtr(effect^.ProcessReplacing)));
    Writeln('ProcessDoubleReplacing address is ', IntToHex(ToIntPtr(effect^.ProcessDoubleReplacing)));
    Writeln('Object address is ', IntToHex(ToIntPtr(effect^.pObject)));
    Writeln('User address is ', IntToHex(ToIntPtr(effect^.User)));
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
    Writeln('---------------- Start dispatcher -------------------');
    effect^.Dispatcher(effect, ord(effOpen), 0, 0, nil, 0);
    effect^.Dispatcher(effect, ord(effSetSampleRate), 0, 0, nil, 44100);
    effect^.Dispatcher(effect, ord(effSetBlockSize), 0, 512, nil, 0);
    effect^.Dispatcher(effect, ord(effMainsChanged), 0, 1, nil, 0);
    Writeln('Vst version is ', effect^.Dispatcher(effect, ord(effGetVstVersion), 0, 0, nil, 0));
    effect^.Dispatcher(effect, ord(effGetEffectName), 0, 0, @buffer, 0);
    Writeln('Effect name is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetVendorString), 0, 0, @buffer, 0);
    Writeln('Vendor string is ', buffer);
    buffer[0] := #0;
    Writeln('Vendor version is ', effect^.Dispatcher(effect, ord(effGetVendorVersion), 0, 0, nil, 0));
    effect^.Dispatcher(effect, ord(effGetProductString), 0, 0, @buffer, 0);
    Writeln('Product string is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetParamName), 0, 0, @buffer, 0);
    Writeln('Parameter 0 name is ', buffer);
    buffer[0] := #0;
    Writeln('Parameter 0 default is ', effect^.GetParameter(effect, 0): 7: 7);
    effect^.Dispatcher(effect, ord(effGetParamLabel), 0, 0, @buffer, 0);
    Writeln('Parameter 0 label is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetParamDisplay), 0, 0, @buffer, 0);
    Writeln('Parameter 0 display is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetProgramName), 0, 0, @buffer, 0);
    Writeln('Current program name is ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effEditGetRect), 0, 0, @rect, 0);
    if Assigned(rect) then
      Writeln('Rect L T R B is ', rect^.Left, ' ', rect^.Top, ' ', rect^.Right, ' ', rect^.Bottom);

    //effect^.Dispatcher(effect, effSetProgram,0,3,nil,0);
    //Writeln('Get program index is ',effect^.Dispatcher(effect, effGetProgram,0,0,nil,0));
    //Writeln('Now param 0 is ',effect^.GetParameter(effect,0));

    // Process test
    effect^.SetParameter(effect, 0, 0.1);
    effect^.Dispatcher(effect, ord(effGetParamName), 0, 0, @buffer, 0);
    Write('Parameter 0: ', 0.1: 3: 3, ' ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetParamDisplay), 0, 0, @buffer, 0);
    Write(' ', buffer);
    buffer[0] := #0;
    effect^.Dispatcher(effect, ord(effGetParamLabel), 0, 0, @buffer, 0);
    Writeln(' ', buffer);
    buffer[0] := #0;

    //effect^.Dispatcher(effect, effStartProcess, 0, 0, nil, 0);
    effect^.ProcessReplacing(effect, inputs, outputs, 10);
    //effect^.Dispatcher(effect, effStopProcess, 0, 0, nil, 0);
    for i := 0 to 9 do
    begin
      Write(i, ' | inputs', ': L=', inputs[0][i]: 3: 3, ' R=', inputs[1][i]: 3: 3);
      Writeln(' | outputs', ': L=', outputs[0][i]: 3: 3, ' R=', outputs[1][i]: 3: 3);
    end;
    effect^.Dispatcher(effect, ord(effClose), 0, 0, nil, 0);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Writeln('----- Press enter to free library and memory ...');
  Readln;
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
