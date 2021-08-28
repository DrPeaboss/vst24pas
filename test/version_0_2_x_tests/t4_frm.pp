unit t4_frm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,ExtCtrls,ComCtrls,StdCtrls,t4_main;

type

  { TFormain }

  TFormain = class(TForm)
    GainLb:TLabel;
    GainTb:TTrackBar;
    OscModeRG:TRadioGroup;
    procedure FormCreate(Sender:TObject);
    procedure GainTbChange(Sender:TObject);
    procedure OscModeRGClick(Sender:TObject);
  private

  public
    Plug:TEasySynth;
  end;

var
  Formain: TFormain;

implementation

uses
  oscillator;

{$R *.lfm}

{ TFormain }

procedure TFormain.OscModeRGClick(Sender:TObject);
begin
  Plug.Osc.OscMode:=TOscModes(OscModeRG.ItemIndex);
end;

procedure TFormain.GainTbChange(Sender:TObject);
var
  Gain:Single;
begin
  Gain:=GainTb.Position / 20000;
  Plug.Param.SetParamAutomated(0,Gain);
  GainLb.Caption:=Format('Gain: %.3f',[Gain*2]);
end;

procedure TFormain.FormCreate(Sender:TObject);
begin
  GainLb.Caption:='Gain: 0.000';
end;

end.

