unit uEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus,ComCtrls, vst24pas.gui;

type

  { TFormGain }

  TFormGain = class(TForm)
    CheckBoxHiFPS:  TCheckBox;
    LabelGain:      TLabel;
    MenuItemReset:  TMenuItem;
    PopupMenuReset: TPopupMenu;
    TrackBarGain:TTrackBar;
    procedure CheckBoxHiFPSChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemResetClick(Sender: TObject);
    procedure TrackBarGainChange(Sender:TObject);
  private
    FLocked: boolean;
    Timer:   TTimer;
    procedure TimerTimer(Sender: TObject);
  public
    Editor: TGuiEditor;
  end;

implementation

uses
  vst24pas.utils;

{$R *.lfm}

{ TFormGain }

procedure TFormGain.TrackBarGainChange(Sender:TObject);
var
  param: single;
begin
  param := TrackBarGain.Position / 1000;
  if not FLocked then
    Editor.GetPlugin.SetParameterAutomated(0, param);
  LabelGain.Caption := format('Gain %.3fdB %.3f', [VstAmp2dB(2 * param), 2 * param]);
end;

procedure TFormGain.TimerTimer(Sender: TObject);
begin
  FLocked := True;
  TrackBarGain.Position := Round(Editor.GetPlugin.GetParameter(0) * 1000);
  FLocked := False;
end;

procedure TFormGain.FormCreate(Sender: TObject);
begin
  LabelGain.Caption := Format('Gain %.3fdB %.3f', [VstAmp2dB(1), 1.0]);
  Timer := TTimer.Create(self);
  Timer.Interval := 40;
  Timer.OnTimer := @TimerTimer;
end;

procedure TFormGain.CheckBoxHiFPSChange(Sender: TObject);
begin
  if CheckBoxHiFPS.Checked then
    Timer.Interval := 20
  else
    Timer.Interval := 40;
end;

procedure TFormGain.MenuItemResetClick(Sender: TObject);
begin
  TrackBarGain.Position := 500;
end;

end.
