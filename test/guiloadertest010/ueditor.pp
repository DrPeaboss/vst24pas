unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,ExtCtrls,StdCtrls,
  vst24pas.core;

type

  { TFormEditor }

  TFormEditor = class(TForm)
    ButtonInfo:TButton;
    PanelEditor:TPanel;
    PanelTools:TPanel;
    Timer:TTimer;
    procedure ButtonInfoClick(Sender:TObject);
    procedure FormClose(Sender:TObject; var CloseAction:TCloseAction);
    procedure FormHide(Sender:TObject);
    procedure FormShow(Sender:TObject);
    procedure TimerTimer(Sender:TObject);
  private

  public
    AEffect:PAEffect;
    ID:Integer;
  end;

implementation

uses
  ufrmmain;

{$R *.lfm}

{ TFormEditor }

procedure TFormEditor.TimerTimer(Sender:TObject);
begin
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditIdle,0,0,nil,0);
end;

procedure TFormEditor.FormClose(Sender:TObject; var CloseAction:TCloseAction);
begin
  if IsConsole then Writeln('Editor Close, ID: ',ID);
  //if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditClose,0,0,nil,0);
  CloseAction:=caNone;
  FormPlugManager.UncheckBox(ID);
end;

procedure TFormEditor.FormHide(Sender:TObject);
begin
  if IsConsole then Writeln('Editor Hide, ID: ',ID);
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditClose,0,0,nil,0);
end;

procedure TFormEditor.ButtonInfoClick(Sender:TObject);
begin
  // TODO
end;

procedure TFormEditor.FormShow(Sender:TObject);
var
  Rect:PERect;
begin
  if IsConsole then Writeln('Editor Show, ID: ',ID);
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditOpen,0,0,FromIntPtr(PanelEditor.Handle),0);
  Rect:=nil;
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditGetRect,0,0,@Rect,0);
  if Assigned(Rect) then
  begin
    self.Width:=Rect^.Right-Rect^.Left;
    self.Height:=Rect^.Bottom-Rect^.Top+PanelTools.Height;
  end;
end;

end.

