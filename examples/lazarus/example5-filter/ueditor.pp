unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type

  { TFormFilter }

  TFormFilter = class(TForm)
    LabelCutoff: TLabel;
    LabelResonance: TLabel;
    RadioGroupAttenuationMode: TRadioGroup;
    RadioGroupFilterMode: TRadioGroup;
    Timer: TTimer;
    TrackBarCutoff:TTrackBar;
    TrackBarResonance: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroupAttenuationModeClick(Sender: TObject);
    procedure RadioGroupFilterModeClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarCutoffChange(Sender: TObject);
    procedure TrackBarResonanceChange(Sender: TObject);
  private
    FLocked:boolean;
  public

  end;

implementation

uses
  vst24pas.utils, umain;

{$R *.lfm}

{ TFormFilter }

procedure TFormFilter.RadioGroupFilterModeClick(Sender: TObject);
begin
  TFilter(gPlugin).Mode := TFilterMode(RadioGroupFilterMode.ItemIndex);
end;

procedure TFormFilter.TimerTimer(Sender: TObject);
begin
  FLocked := true;
  TrackBarCutoff.Position := round(10000*gPlugin.GetParameter(0));
  TrackBarResonance.Position := round(10000*gPlugin.GetParameter(1));
  FLocked := false;
end;

procedure TFormFilter.FormCreate(Sender: TObject);
begin
  timer.Enabled := false;
  timer.Interval := 20;
  LabelCutoff.Caption := 'Cutoff 0.9900';
  LabelResonance.Caption := 'Resonance 0.0100';
end;

procedure TFormFilter.FormHide(Sender: TObject);
begin
  timer.Enabled := false;
end;

procedure TFormFilter.FormShow(Sender: TObject);
begin
  timer.Enabled := true;
end;

procedure TFormFilter.RadioGroupAttenuationModeClick(Sender: TObject);
begin
  TFilter(gPlugin).AttenuationMode := TAttenuationMode(RadioGroupAttenuationMode.ItemIndex);
end;

procedure TFormFilter.TrackBarCutoffChange(Sender: TObject);
var
  cutoff:double;
begin
  cutoff:=TrackBarCutoff.Position/10000;
  if not FLocked then gPlugin.SetParameterAutomated(0,cutoff);
  LabelCutoff.Caption := Format('Cutoff %.4f',[cutoff]);
end;

procedure TFormFilter.TrackBarResonanceChange(Sender: TObject);
var
  res:double;
begin
  res:=TrackBarResonance.Position/10000;
  if not FLocked then gPlugin.SetParameterAutomated(1,res);
  LabelResonance.Caption := Format('Resonance %.4f',[res]);
end;

end.
