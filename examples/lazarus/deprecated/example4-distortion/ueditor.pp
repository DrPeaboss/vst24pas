unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, vst24pas.gui;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelAmp: TLabel;
    LabeldB: TLabel;
    LabelShow: TLabel;
    Timer:     TTimer;
    TrackBarThreshold: TTrackBar;
    TrackBarThresholddB: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarThresholdChange(Sender: TObject);
    procedure TrackBarThresholddBChange(Sender: TObject);
  private
    FLocked: boolean;
  public
    Editor: TGuiEditor;
  end;

implementation

uses
  vst24pas.utils;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Timer.Enabled := False;
  FLocked := True;
  LabelShow.Caption := Format('Threshold %.3fdB %.4f', [VstAmp2dB(0.9), 0.9]);
  TrackBarThresholddB.Position := TrackBarThresholddB.Max + Round(100 * VstAmp2dB(0.9));
end;

procedure TFormMain.FormHide(Sender: TObject);
begin
  timer.Enabled := False;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  timer.Enabled := True;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  FLocked := True;
  TrackBarThreshold.Position := Round(Editor.GetPlugin.GetParameter(0) * TrackBarThreshold.Max);
  TrackBarThresholddB.Position := TrackBarThresholddB.Max + Round(100 * VstAmp2dB(Editor.GetPlugin.GetParameter(0)));
  FLocked := False;
end;

procedure TFormMain.TrackBarThresholdChange(Sender: TObject);
var
  d: double;
begin
  d := TrackBarThreshold.Position / TrackBarThreshold.Max;
  if not FLocked then
    Editor.GetPlugin.SetParameterAutomated(0, d);
  LabelShow.Caption := Format('Threshold %.3fdB %.4f', [VstAmp2dB(d), d]);
end;

procedure TFormMain.TrackBarThresholddBChange(Sender: TObject);
var
  d: double;
begin
  d := VstdB2Amp((TrackBarThresholddB.Position - TrackBarThresholddB.Max) / 100);
  if not FLocked then
    Editor.GetPlugin.SetParameterAutomated(0, d);
end;

end.
