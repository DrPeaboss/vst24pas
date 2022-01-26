unit veditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,StdCtrls,
  vmain;

type

  { TFormain }

  TFormain = class(TForm)
    ButtonClearLog:TButton;
    LogMemo:TMemo;
    procedure ButtonClearLogClick(Sender:TObject);
  private
    procedure log(const s:String);inline;
  public
    Plug:TPlug;
    procedure Idle;
  end;

var
  Formain: TFormain;

implementation

{$R *.lfm}

{ TFormain }

procedure TFormain.ButtonClearLogClick(Sender:TObject);
begin
  LogMemo.Lines.Clear;
end;

procedure TFormain.log(const s:String);
begin
  LogMemo.Lines.Add(s);
end;

procedure TFormain.Idle;
var
  lst:TStringList;
  i:Integer;
begin
  lst:=Plug.LogList;
  if lst.Count>0 then
  begin
    for i:=0 to lst.Count-1 do
      log(lst.Strings[i]);
    lst.Clear;
  end;
end;

end.

