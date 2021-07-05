unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    Timer: TTimer;
    TrackBarGain: TTrackBar;
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarGainChange(Sender: TObject);
  private
    FLocked:boolean;
  public

  end;

implementation

uses
  vst24pas.utils;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TrackBarGainChange(Sender: TObject);
begin
  if not FLocked then
    gPlugin.SetParameterAutomated(0,TrackBarGain.Position/2000);
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  FLocked:=True;
  TrackBarGain.Position := Round(gPlugin.GetParameter(0)*2000);
  FLocked:=False;
end;

end.

