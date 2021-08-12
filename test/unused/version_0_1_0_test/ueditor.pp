unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls,
  umain;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelGain: TLabel;
    LabelInput: TLabel;
    LabelInputShowDelay: TLabel;
    LabelOutput: TLabel;
    Timer: TTimer;
    TrackBarGain: TTrackBar;
    TrackBarInputDelay: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarGainChange(Sender: TObject);
    procedure TrackBarInputDelayChange(Sender: TObject);
  private
    FLocked:boolean;
    Gain:single;
  public
    Plugin:TNewGain;
  end;

implementation

uses
  vst24pas.utils;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TrackBarGainChange(Sender: TObject);
begin
  Gain:=TrackBarGain.Position/2000;
  if not FLocked then
    Plugin.SetParameterAutomated(0,Gain);
  LabelGain.Caption := Format('Gain %.3fdB %.3f',[VstAmp2dB(Gain),Gain]);
end;

procedure TFormMain.TrackBarInputDelayChange(Sender: TObject);
begin
  LabelInputShowDelay.Caption := Format('ShowDelay %d ms',[TrackBarInputDelay.Position*10]);
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  Gain:=Plugin.GetParameter(0)*2;
  FLocked:=True;
  TrackBarGain.Position := Round(Gain*1000);
  LabelInput.Caption := Format('I: L %.5fdB %.5f  R %.5fdB %.5f',
    [VstAmp2dB(Plugin.PeakL),Plugin.PeakL,VstAmp2dB(Plugin.PeakR),Plugin.PeakR]);
  LabelOutput.Caption := Format('O: L %.5fdB %.5f  R %.5fdB %.5f',
    [VstAmp2dB(Plugin.PeakL*Gain),Plugin.PeakL*Gain,VstAmp2dB(Plugin.PeakR*Gain),Plugin.PeakR*Gain]);
  FLocked:=False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LabelGain.Caption := Format('Gain %.3fdB %.3f',[VstAmp2dB(1.0),1.0]);
end;

end.

