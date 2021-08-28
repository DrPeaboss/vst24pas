{ Thanks to
http://www.martin-finke.de/blog/articles/audio-plugins-009-receiving-midi/ }

unit t4_main;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui,oscillator;

type

  { TEasySynth }

  TEasySynth = class(TVGuiPlugin)
  private
    FOsc:TSingleOsc;
    FNum:Integer;
    FGain:Single;
    procedure OnMidiIn(cmd,note,velo:Byte);
  protected
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
  public
    constructor Create(AHost:THostCallback);override;
    destructor Destroy;override;
    property Osc:TSingleOsc read FOsc;
  end;

implementation

uses
  t4_frm;

{ TEasySynth }

procedure TEasySynth.OnMidiIn(cmd,note,velo:Byte);
begin
  if cmd=$90 then
  begin
    Inc(FNum);
    FOsc.Freq:=NoteToFrequency(note);
    FOsc.Gain:=velo / 127;
  end else if cmd=$80 then
    Dec(FNum);
  FOsc.IsMuted:=FNum=0;
end;

procedure TEasySynth.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
begin
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=FOsc.NextSample*FGain*2;
    outputs[1,i]:=outputs[0,i];
  end;
end;

constructor TEasySynth.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  Base.SetUniqueID('PZn6');
  Base.SetNames('EasySynth','PeaZomboss','vst24pas: v02xtest4');
  Base.SetVersion($020006,$020006);
  Base.SetAsSynth;
  Param.AddParameter(0.5,'Gain','');
  Param.BindLastParameter(FGain);
  Editor.SetGui(TFormain);
  FOsc:=TSingleOsc.Create(omSine);
  TFormain(Editor.Gui).Plug:=self;
  MIDI.SetOnMidiIn(@OnMidiIn);
end;

destructor TEasySynth.Destroy;
begin
  FOsc.Free;
  inherited Destroy;
end;

end.

