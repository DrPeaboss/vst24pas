unit vmain;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui,classes;

type

  { TPlug }

  TPlug=class(TVGuiPlugin)
  private
    FMidiEventCount:Integer;
    FMidiSysexCount:Integer;
    FLogList:TStringList;
  protected
    function Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;override;
  public
    constructor Create(AHost:THostCallback);override;
    destructor Destroy;override;
    procedure ProcessEvents(Events:PVstEvents);
    procedure ProcessMidiEvent(const MidiEvent:TVstMidiEvent);
    procedure ProcessMidiSysex(const SysexEvent:TVstMidiSysexEvent);
    property LogList:TStringList read FLogList;
  end;

implementation

uses
  veditor,sysutils;

{ TPlug }

function TPlug.Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  if opcode=effProcessEvents then
    if Assigned(ptr) then
      ProcessEvents(ptr);
  Result:=inherited Dispatcher(opcode,index,value,ptr,opt);
end;

constructor TPlug.Create(AHost:THostCallback);
var
  frm:TFormain;
begin
  inherited Create(AHost);
  with Base do
  begin
    SetAsSynth;
    SetUniqueID('PZt1');
    SetCanDo(pcdReceiveVstMidiEvent);
    SetVersion(0,0);
    SetNames('MidiReceiveTest','PeaZomboss','vst24pas: test');
  end;
  Editor.SetGui(TFormain);
  frm:=TFormain(Editor.Gui);
  Editor.SetIdle(@frm.Idle);
  frm.Plug:=Self;
  FLogList:=TStringList.Create;
end;

destructor TPlug.Destroy;
begin
  FLogList.Free;
  inherited Destroy;
end;

procedure TPlug.ProcessEvents(Events:PVstEvents);
var
  i:Integer;
begin
  for i:=0 to Events^.NumEvents-1 do
  begin
    case Events^.Events[i]^.Typ of
      kVstMidiType: ProcessMidiEvent(PVstMidiEvent(Events^.Events[i])^);
      kVstSysExType: ProcessMidiSysex(PVstMidiSysexEvent(Events^.Events[i])^);
    end;
  end;
end;

{
+----------------------------------------+-------+--------------------------+----------------+
| 关闭音符                               | 80+CH | 音符音高                 | 关闭力度       |
+----------------------------------------+-------+--------------------------+----------------+
| 开启音符                               | 90+CH | 音符音高                 | 打开力度       |
| 触后压力                               | A0+CH | 音符音高                 | 触后压力值     |
| 控制器                                 | B0+CH | 控制器编号               | 控制的数值     |
| 音色切换                               | C0+CH | 音色编号                 | -              |
| 通道压力                               | D0+CH | 该通道全部键盘的触后压力 | -              |
| 弯音轮                                 | E0+CH | 弯音轮低位数据           | 弯音轮高位数据 |
| 系统普通信息、实时信息、及高级信息代码 | F0+CH | 0或若干个数据字节        | -              |
+----------------------------------------+-------+--------------------------+----------------+
}

procedure TPlug.ProcessMidiEvent(const MidiEvent:TVstMidiEvent);
begin
  // Should not operate GUI thread in process thread
  FLogList.Add('%d: Got Midi Event -->ctrl:%X, note:%d, velo:%d, detune:%d',
    [FMidiEventCount,MidiEvent.MidiData[0],MidiEvent.MidiData[1],MidiEvent.MidiData[2],MidiEvent.Detune]);
  inc(FMidiEventCount);
end;

procedure TPlug.ProcessMidiSysex(const SysexEvent:TVstMidiSysexEvent);
begin
  FLogList.Add(Format('%d Got Midi Sysex Event, ByteSize: %d',
    [FMidiSysexCount,SysexEvent.ByteSize]));
  inc(FMidiSysexCount);
end;

end.

