unit umain;

{$mode objfpc}{$H+}

interface

uses
  vst2pluginbase, vst2interfaces;

const
  BigNum = 100000000;

type

  { TMyPlugin }

  TMyPlugin = class(TVSTPlugin)
  private
    FProcess32Count:integer;
    FIdleCount:integer;
    // Called by host, similar to timer, but usually have low fps
    procedure EditIdle;
    function CustomParamDisplay(index:integer):string;
  public
    constructor Create(VstHost: THostCallback); override;
    procedure Process32(const inputs, outputs: TBuffer32; sampleframes: integer); override;
    function Dispatcher(opcode: TAEffectOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
      opt: single): IntPtr; override;
    property Process32Count:integer read FProcess32Count;
  end;

implementation

uses
  ueditor, SysUtils, TypInfo, Forms;

{ TMyPlugin }

procedure TMyPlugin.EditIdle;
begin
  Inc(FIdleCount);
  TFormMain(Editor.Gui).LabelIdleCount.Caption := IntToStr(FIdleCount);
end;

constructor TMyPlugin.Create(VstHost: THostCallback);
begin
  inherited Create(VstHost);
  PlugInitEffectInfo('v020testPlugin', 'PeaZomboss', 'test', 20, kPlugCategEffect);
  PlugInitParamInfo(0, 0.5, 'Gain', 'dB', pdmCustom);
  PlugInitPreset(0, 'Preset 0', [0.5]);
  PlugInitPreset(1, 'Preset 1', [1.0]);
  PlugInitPreset(2, 'Preset 2', [0.66]);
  PlugInitPreset(3, 'Preset 3', [0.33]);
  SetUniqueID('P', 'Z', 'n', '2');
  SetVersion(20);
  SetEditor(TFormMain);
  SetEffectFlag(effFlagsProgramChunks);
  TFormMain(Editor.Gui).Plugin:=self;
  Editor.SetIdleProc(@EditIdle); // Here enable the 'timer', set nil to disable it
  OnCustomParamDisplay := @CustomParamDisplay;
end;

procedure TMyPlugin.Process32(const inputs, outputs: TBuffer32; sampleframes: integer);
var
  gain: single;
  i: integer;
begin
  inc(FProcess32Count);
  gain := Parameters[0] * 2;
  for i := 0 to sampleframes - 1 do
  begin
    outputs[0, i] := gain * inputs[0, i];
    outputs[1, i] := gain * inputs[1, i];
  end;
end;

function TMyPlugin.CustomParamDisplay(index: integer): string;
begin
  if index = 0 then
    Result := Float2String(VstAmp2dB(2*Parameters[0])) // Our custom display
  else
    Result := '';
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
  // ListView will clear items when hide the form, so we must stop changing the item
  if Editor.IsOpen then
    f.ListView1.Items.Item[Ord(opcode)].SubItems[0] := IntToStr(f.opTimes[Ord(opcode)]);
  if (opcode = effEditIdle) and f.BlockEditIdle then exit;
  s := format('opcode:%s index:%d value:%d ptr:%p opt:%.2f',
    [GetEnumName(TypeInfo(TAEffectOpcodes), Ord(opcode)), index, Value, ptr, opt]);
  if opcode = effCanDo then
    f.MemoLog.Lines.Add('Got cando string: ' + strpas(ptr));
  f.MemoLog.Lines.Add(s);
end;

end.
