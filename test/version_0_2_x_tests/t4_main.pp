{ Thanks to
http://www.martin-finke.de/blog/articles/audio-plugins-009-receiving-midi/ }

unit t4_main;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui,oscillator,classes;

type

  generic TVoice<T> = class(specialize TOscillator<T>)
  public
    note:Byte;

  end;

  { TEasySynth }

  TEasySynth = class(TVGuiPlugin)
  private type
    TOsc = specialize TVoice<Single>;
  private const
    MaxPoly = 8;
  private
    FGain:Single;
    FVoiceNum:Integer;
    FNoteNum:Integer;
    FVoices:array[0..MaxPoly-1] of TOsc;
    FLogList:TStringList;
    procedure OnMidiIn(cmd,note,velo:Byte);
  protected
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
    function Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;override;
  public
    constructor Create(AHost:THostCallback);override;
    destructor Destroy;override;
    procedure NoteOn(note,velo:Byte);
    procedure NoteOff(note:Byte);
    procedure AllNoteOff;
    procedure SetOscMode(mode:Integer);
    function NextSample:Single;
    property LogList:TStringList read FLogList;
    property Gain:Single read FGain;
  end;

implementation

uses
  t4_frm,sysutils;

{ TEasySynth }

procedure TEasySynth.OnMidiIn(cmd,note,velo:Byte);
begin
  if cmd=$90 then
  begin
    NoteOn(note,velo);
    FLogList.Add(Format('Note ON  %d %d',[note,velo]));
  end else if cmd=$80 then
  begin
    NoteOff(note);
    FLogList.Add(Format('Note OFF %d',[note]));
  end
  else if cmd=$b0 then
  begin
    if (note=$7a) or (note=$7e) or (note=$7b) then
    begin
      AllNoteOff;
      FLogList.Add('MUTED');
    end;
  end;
end;

procedure TEasySynth.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
  next,tGain:Single;
begin
  tGain:=FGain*2;
  for i:=0 to SampleFrames-1 do
  begin
    next:=NextSample;
    outputs[0,i]:=next*tGain;
    outputs[1,i]:=outputs[0,i];
  end;
end;

function TEasySynth.Dispatcher(opcode:TAEOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
var
  i:Integer;
begin
  if opcode=effSetSampleRate then
    for i:=0 to MaxPoly-1 do
      FVoices[i].SampleRate:=opt;
  Result:=inherited Dispatcher(opcode,index,value,ptr,opt);
end;

constructor TEasySynth.Create(AHost:THostCallback);
var
  i:Integer;
  Frm:TFormain;
begin
  inherited Create(AHost);
  Base.SetUniqueID('PZn6');
  Base.SetNames('EasySynth','PeaZomboss','vst24pas: v02xtest4');
  Base.SetVersion($020006,$020006);
  Base.SetAsSynth;
  Param.AddParameter(0.5,'Gain','');
  Param.BindLastParameter(FGain);
  Preset.AddPreset('Default',[0.5]);
  Editor.SetGui(TFormain);
  Frm:=TFormain(Editor.Gui);
  Editor.SetIdle(@Frm.Idle);
  Frm.Plug:=self;
  MIDI.SetOnMidiIn(@OnMidiIn);
  for i:=0 to MaxPoly-1 do
    FVoices[i]:=TOsc.Create(omSine);
  FLogList:=TStringList.Create;
end;

destructor TEasySynth.Destroy;
var
  i:Integer;
begin
  for i:=0 to MaxPoly-1 do
    FVoices[i].Free;
  FLogList.Free;
  inherited Destroy;
end;

procedure TEasySynth.NoteOn(note,velo:Byte);
var
  i:Integer;
begin
  if FVoiceNum<MaxPoly then
  begin
    for i:=0 to FVoiceNum do
    begin
      if FVoices[i].IsMuted then
      begin
        FVoices[i].note:=note;
        FVoices[i].Freq:=NoteToFrequency(note);
        FVoices[i].Gain:=velo/127;
        FVoices[i].IsMuted:=False;
        FVoices[i].Reset;
        Break;
      end;
    end;
    Inc(FVoiceNum);
  end else
  begin
    i:=FNoteNum mod MaxPoly;
    FVoices[i].note:=note;
    FVoices[i].Freq:=NoteToFrequency(note);
    FVoices[i].Gain:=velo/127;
    FVoices[i].Reset;
  end;
  Inc(FNoteNum);
end;

procedure TEasySynth.NoteOff(note:Byte);
var
  i:Integer;
begin
  if FVoiceNum>0 then
  begin
    for i:=0 to MaxPoly-1 do
    begin
      if (FVoices[i].note=note) and not FVoices[i].IsMuted then
      begin
        //FVoices[i].IsMuted:=True;
        FVoices[i].Release;
        Dec(FVoiceNum);
        Break;
      end;
    end;
    if FVoiceNum=0 then FNoteNum:=0;
  end;
end;

procedure TEasySynth.AllNoteOff;
var
  i:Integer;
begin
  FNoteNum:=0;
  FVoiceNum:=0;
    for i:=0 to MaxPoly-1 do
      FVoices[i].IsMuted:=True;
end;

procedure TEasySynth.SetOscMode(mode:Integer);
var
  i:Integer;
begin
  for i:=0 to MaxPoly-1 do
    FVoices[i].OscMode:=TOscModes(mode);
end;

function TEasySynth.NextSample:Single;
var
  i:Integer;
begin
  Result:=0;
  for i:=0 to MaxPoly-1 do
  begin
    Result:=Result+FVoices[i].NextSample;
  end;
  Result:=Result*0.4;
end;

end.

