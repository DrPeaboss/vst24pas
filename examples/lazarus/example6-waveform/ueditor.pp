unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  umain;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelDelay: TLabel;
    PaintBox:       TPaintBox;
    Timer:          TTimer;
    ToggleBoxPause: TToggleBox;
    TrackBarAccuracy: TTrackBar;
    procedure PaintBoxPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure ToggleBoxPauseChange(Sender: TObject);
    procedure TrackBarAccuracyChange(Sender: TObject);
  private

  public
    Plugin:TWaveForm;
  end;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.ToggleBoxPauseChange(Sender: TObject);
begin
  timer.Enabled := not ToggleBoxPause.Checked;
end;

procedure TFormMain.TrackBarAccuracyChange(Sender: TObject);
begin
  TWaveForm(Plugin).Accuracy := TrackBarAccuracy.Position;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  PaintBox.Refresh;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var
  info: PBufInfo;
  i: integer;
  Start0, Start1: TPoint; // Relative to PaintBox
  Y0, Y1, bufid: integer;
begin
  info := @TWaveForm(Plugin).Info;
  if info^.CanBuf0 then bufid:=0 else bufid:=1; // We read the buffer
  Start0.Create(0, PaintBox.Height div 4);
  Start1.Create(0, Start0.Y + PaintBox.Height div 2);
  PaintBox.Canvas.MoveTo(Start0);
  PaintBox.Canvas.LineTo(PaintBox.Width, Start0.Y);
  PaintBox.Canvas.MoveTo(Start1);
  PaintBox.Canvas.LineTo(PaintBox.Width, Start1.Y);
  Y0 := Start0.Y - Round(125 * info^.I0[bufid,0]);
  Y1 := Start1.Y - Round(125 * info^.I1[bufid,0]);
  Inc(Start0.x,(PaintBox.Width-512) div 2);
  Inc(Start1.x,(PaintBox.Width-512) div 2);
  for i := 1 to 512 do
  begin
    // Draw left channel
    PaintBox.Canvas.MoveTo(Start0.X, Y0);
    Inc(Start0.X, 1);
    Y0 := Start0.Y - Round(125 * info^.I0[bufid,i]);
    PaintBox.Canvas.LineTo(Start0.X, Y0);
    // Draw right channel
    PaintBox.Canvas.MoveTo(Start1.X, Y1);
    Inc(Start1.X, 1);
    Y1 := Start1.Y - Round(125 * info^.I1[bufid,i]);
    PaintBox.Canvas.LineTo(Start1.X, Y1);
  end;
end;

end.

