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

procedure TEasySynth.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
  Command,Note,Velocity:Byte;
begin
  for i:=0 to SampleFrames-1 do
  begin
    if MIDI.QueryNewMidiData(Command,Note,Velocity) then
    begin
      if Command=$90 then
      begin
        FOsc.IsMuted:=False;
        FOsc.Gain:=Velocity / 127;
        FOsc.Freq:=NoteToFrequency(Note);
      end
      else if Command=$80 then
        FOsc.IsMuted:=True;
    end;
    outputs[0,i]:=FOsc.NextSample;
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
  Editor.SetGui(TFormain);
  FOsc:=TSingleOsc.Create(omSine);
  TFormain(Editor.Gui).Plug:=self;
end;

destructor TEasySynth.Destroy;
begin
  FOsc.Free;
  inherited Destroy;
end;

end.

