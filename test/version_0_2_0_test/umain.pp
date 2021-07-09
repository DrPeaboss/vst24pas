unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst2pluginbase, vst2interfaces;

type

  { TMyPlugin }

  TMyPlugin = class(TVSTPlugin)
  public
    constructor Create(VstHost: THostCallback); override;
    procedure Process32(const inputs, outputs: TBuffer32; sampleframes: integer); override;
    function GetParamDisplay(index: integer): string; override;
    function Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
      opt: single): IntPtr; override;
  end;

var
  gPlugin: TMyPlugin;

implementation

uses
  ueditor, SysUtils, Math, TypInfo, Forms;

{ TMyPlugin }

constructor TMyPlugin.Create(VstHost: THostCallback);
begin
  inherited Create(VstHost);
  PlugInitEffectInfo('v020testplugin', 'PeaZomboss', 'test', 20, kPlugCategEffect);
  PlugInitParamInfo(0, 0.5, 'Gain', 'dB', pdmCustom);
  PlugInitPreset(0, 'Preset 0', [0.5]);
  PlugInitPreset(1, 'Preset 1', [1.0]);
  SetUniqueID('P', 'Z', 'n', '2');
  SetVersion(20);
  SetEditor(TPluginEditor.Create(self, TFormMain));
  gPlugin := self; // Set global variant for ueditor
end;

procedure TMyPlugin.Process32(const inputs, outputs: TBuffer32; sampleframes: integer);
var
  gain: single;
  i: integer;
begin
  gain := Parameters[0] * 2;
  for i := 0 to sampleframes - 1 do
  begin
    outputs[0, i] := gain * inputs[0, i];
    outputs[1, i] := gain * inputs[1, i];
  end;
end;

function TMyPlugin.GetParamDisplay(index: integer): string;
begin
  if index = 0 then
  begin
    Result := Format('%.7f', [20 * log10(2 * Parameters[0])]); // Our custom display
  end else
    Result := inherited GetParamDisplay(index);
end;

function TMyPlugin.Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer; opt: single): IntPtr;
var
  s: string;
  f: TFormMain;
begin
  Result := inherited Dispatcher(opcode, index, Value, ptr, opt);
  f := tformmain(Editor.Gui);
  Inc(f.opTimes[Ord(opcode)]);
  if f.ToggleBoxPause.Checked then exit; // Stop changing UI
  if f.MemoLog.Lines.Count > 1000 then
    f.MemoLog.Clear;
  s := format('opcode:%s index:%d value:%d ptr:%p opt:%.2f',
    [GetEnumName(TypeInfo(TAEffectOpcodes), Ord(opcode)), index, Value, ptr, opt]);
  if opcode = effCanDo then
    f.MemoLog.Lines.Add('Got cando string: ' + strpas(ptr));
  f.MemoLog.Lines.Add(s);
  // ListView will clear items when hide the form, so we must stop changing the item
  if Editor.IsOpen then
    f.ListView1.Items.Item[Ord(opcode)].SubItems[0] := IntToStr(f.opTimes[Ord(opcode)]);
end;

end.
