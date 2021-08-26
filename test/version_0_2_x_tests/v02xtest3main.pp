{
Thanks to
http://www.martin-finke.de/blog/articles/audio-plugins-008-synthesizing-waveforms/
}

unit v02xtest3main;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui,oscillator;

type
  { TV02XTest3 }

  TV02XTest3 = class(TVGuiPlugin)
  private
    FOsc:TSingleOsc;
  protected
    function Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;override;
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
  public
    constructor Create(AHost:THostCallback);override;
    destructor Destroy;override;
    property Osc:TSingleOsc read FOsc;
  end;

implementation

uses
  v02xtest3editor;

{ TV02XTest3 }

function TV02XTest3.Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  if opcode=effSetSampleRate then FOsc.SampleRate:=opt;
  Result:=inherited Dispatcher(opcode,index,value,ptr,opt);
end;

procedure TV02XTest3.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
begin
  FOsc.Generage(outputs[0],SampleFrames);
  for i:=0 to SampleFrames-1 do
    outputs[1][i]:=outputs[0][i];
end;

constructor TV02XTest3.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  Base.SetUniqueID('PZn5');
  Base.SetNames('v02xtest3','PeaZomboss','vst24pas: v02xtest3');
  Base.SetVersion($00020005,$00020005);
  Base.SetAsSynth;
  Editor.SetGui(TFormain);
  TFormain(Editor.Gui).Plugin:=self;
  FOsc:=TSingleOsc.Create(omTriangle);
end;

destructor TV02XTest3.Destroy;
begin
  FOsc.Free;
  inherited Destroy;
end;

end.

