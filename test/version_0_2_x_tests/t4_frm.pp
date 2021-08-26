unit t4_frm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,ExtCtrls,t4_main;

type

  { TFormain }

  TFormain = class(TForm)
    OscModeRG:TRadioGroup;
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

end.

