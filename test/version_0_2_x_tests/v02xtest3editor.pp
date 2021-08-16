unit v02xtest3editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,StdCtrls,v02xtest3main;

type

  { TFormain }

  TFormain = class(TForm)
    BtnGetDir:TButton;
    procedure BtnGetDirClick(Sender:TObject);
  private

  public
    Plugin:TV02XTest3;
  end;


implementation

uses
  vst2intf;

{$R *.lfm}

{ TFormain }

procedure TFormain.BtnGetDirClick(Sender:TObject);
var
  curdir:String;
begin
  curdir:=strpas(FromIntPtr(Plugin.Base.CallHost(amGetDirectory)));
  ShowMessage(curdir);
end;

end.

