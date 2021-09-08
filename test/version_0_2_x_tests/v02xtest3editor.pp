unit v02xtest3editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,StdCtrls,ExtCtrls,ComCtrls,v02xtest3main;

type

  { TFormain }

  TFormain = class(TForm)
    BtnPlayAShot:TButton;
    lblFreq:TLabel;
    lblGain:TLabel;
    rgWav:TRadioGroup;
    tbFreq:TTrackBar;
    tbGain:TTrackBar;
    tbPlayPause:TToggleBox;
    Timer:TTimer;
    procedure BtnPlayAShotClick(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure rgWavClick(Sender:TObject);
    procedure tbFreqChange(Sender:TObject);
    procedure tbGainChange(Sender:TObject);
    procedure tbPlayPauseChange(Sender:TObject);
    procedure TimerTimer(Sender:TObject);
  private
    FCount:Integer;
    FPersistentPlaying:Boolean;
  public
    Plugin:TV02XTest3;
  end;


implementation

uses
  vst2utils,oscillator;

{$R *.lfm}

{ TFormain }

procedure TFormain.rgWavClick(Sender:TObject);
begin
  Plugin.Osc.OscMode:=TOscModes(rgWav.ItemIndex);
end;

procedure TFormain.tbFreqChange(Sender:TObject);
begin
  lblFreq.Caption:=Format('Frequency: %d HZ',[tbFreq.Position]);
  with Plugin.Osc do Freq:=tbFreq.Position;
end;

procedure TFormain.tbGainChange(Sender:TObject);
var
  gain:Double;
begin
  gain:=tbGain.Position/10000;
  lblGain.Caption:=Format('Gain: %.3f dB',[VstAmp2dB(gain)]);
  Plugin.Osc.Gain:=gain;
end;

procedure TFormain.tbPlayPauseChange(Sender:TObject);
begin
  with Plugin.Osc do IsMuted:=not tbPlayPause.Checked;
  FPersistentPlaying:=tbPlayPause.Checked;
end;

procedure TFormain.BtnPlayAShotClick(Sender:TObject);
begin
  Plugin.Osc.IsMuted:=False;
  FCount:=0;
end;

procedure TFormain.FormCreate(Sender:TObject);
begin
  lblFreq.Caption:='Frequency: 440 HZ';
  lblGain.Caption:='Gain: 0.000 dB';
end;

procedure TFormain.TimerTimer(Sender:TObject);
begin
  rgWav.ItemIndex:=Ord(Plugin.Osc.OscMode);
  Inc(Fcount);
  if FPersistentPlaying then Exit;
  with Plugin.Osc do
  if (FCount>30) then IsMuted:=True;
end;

end.

