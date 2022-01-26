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
    MemoLog:TMemo;
    OscModeRG:TRadioGroup;
    procedure FormCreate(Sender:TObject);
    procedure GainTbChange(Sender:TObject);
    procedure OscModeRGClick(Sender:TObject);
  private
    FLock:Boolean;
    procedure Log(const s:String);
    procedure Log(const s:String;const fmt:Array of const);
  public
    Plug:TEasySynth;
    procedure Idle;
  end;

var
  Formain: TFormain;

implementation

{$R *.lfm}

{ TFormain }

procedure TFormain.OscModeRGClick(Sender:TObject);
begin
  Plug.SetOscMode(OscModeRG.ItemIndex);
end;

procedure TFormain.Log(const s:String);
begin
  MemoLog.Lines.Add(s);
end;

procedure TFormain.Log(const s:String;const fmt:array of const);
begin
  MemoLog.Lines.Add(s,fmt);
end;

procedure TFormain.Idle;
var
  i:Integer;
begin
  if Plug.LogList.Count>0 then
  begin
    for i:=0 to Plug.LogList.Count-1 do
      log(Plug.LogList.Strings[i]);
    Plug.LogList.Clear;
  end;
  FLock:=True;
  GainTb.Position:=Round(20000*Plug.Gain);
  FLock:=False;
end;

procedure TFormain.GainTbChange(Sender:TObject);
var
  Gain:Single;
begin
  Gain:=GainTb.Position / 20000;
  if not FLock then
    Plug.Param.SetParamAutomated(0,Gain);
  GainLb.Caption:=Format('Gain: %.3f',[Gain*2]);
end;

procedure TFormain.FormCreate(Sender:TObject);
begin
  GainLb.Caption:='Gain: 1.000';
end;

end.

