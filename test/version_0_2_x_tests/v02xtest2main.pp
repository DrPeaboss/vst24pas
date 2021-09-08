unit v02xtest2main;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui;

type

  { TTestPlugin2 }

  TTestPlugin2=class(TVGuiPlugin)
  private
    function GetParamDisplay(index:integer):string;
  protected
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
    function Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;override;
  public
    constructor Create(AHost:THostCallback);override;
  end;

implementation

uses
  v02xtest2editor,sysutils,vst2utils;

{ TTestPlugin2 }

constructor TTestPlugin2.Create(AHost:THostCallback);
var
  Gui:TFormTest2;
begin
  inherited Create(AHost);   // Do not forget
  Base.SetUniqueID('PZn4');  // It's best to set first
  Base.SetVersion(40,40);
  Base.SetNames('v02xtest2','PeaZomboss','vst24pas: v02xtest2');
  Param.AddParameter(0.5,'Gain','dB');
  Param.SetCustomParamDisplay(@GetParamDisplay);
  Preset.AddPreset('Preset 0: default',[0.5]);
  Preset.AddPreset('Preset 1: doubled',[1.0]);
  Preset.AddPreset('Preset 2: silence',[0.0]);
  Editor.SetGui(TFormTest2); // You can't access Editor.Gui if not called SetGui first
  Gui:=Editor.Gui as TFormTest2;
  Gui.Plugin:=self;
  Editor.SetIdle(@Gui.Idle); // Have to set here
end;

function TTestPlugin2.Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
var
  gui:TFormTest2;
begin
  Result:=inherited Dispatcher(opcode,index,value,ptr,opt);
  gui:=Editor.Gui as TFormTest2;
  case opcode of
    effSetProgram: gui.logln('Host set preset: %d',[value]);
    effSetProgramName: gui.logln('Host set preset name: %s',[strpas(ptr)]);
    effSetSampleRate: gui.logln('Host set sample rate: %.5f',[opt]);
    effSetBlockSize: gui.logln('Host set block size: %d',[value]);
    effMainsChanged: if value=0 then gui.logln('Host turn off the plugin')
                     else gui.logln('Host turn on the plugin');
    effGetChunk: gui.logln('Host get chunk');
    effSetChunk: gui.logln('Host set chunk, size: %d',[value]);
    effCanDo: gui.logln('Host ask can do: %s',[StrPas(ptr)]);
    effSetEditKnobMode: gui.logln('Host set edit knob mode: %d',[value]);
    effSetPanLaw: gui.logln('Host set pan law: %d, gain: %.5f',[value,opt]);
    effSetProcessPrecision: gui.logln('Host set process precision: %d',[value]);
  end;
end;

function TTestPlugin2.GetParamDisplay(index:integer):string;
begin
  case index of
    0: Result:=VstAmp2dBString(2*Param.Items[0]);
    else Result:='';
  end;
end;

procedure TTestPlugin2.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
  gain:single;
begin
  gain:=2*Param.Items[0];
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=inputs[0,i]*gain;
    outputs[1,i]:=inputs[1,i]*gain;
  end;
end;

end.

