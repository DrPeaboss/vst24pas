unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,ExtCtrls,StdCtrls,
  vst24pas.core,upluginfo;

type

  { TFormEditor }

  TFormEditor = class(TForm)
    PanelMain:TPanel;
    PanelTools:TPanel;
    Timer:TTimer;
    ToggleBoxInfo:TToggleBox;
    ToggleBoxPlug:TToggleBox;
    procedure FormClose(Sender:TObject; var CloseAction:TCloseAction);
    procedure FormCreate(Sender:TObject);
    procedure FormHide(Sender:TObject);
    procedure FormShow(Sender:TObject);
    procedure TimerTimer(Sender:TObject);
    procedure ToggleBoxInfoClick(Sender:TObject);
    procedure ToggleBoxPlugClick(Sender:TObject);
  private
    FPluginfo:TFramePluginfo;
    FEditorOpening:Boolean;
    procedure OpenEditor;
    procedure CloseEditor;
  public
    AEffect:PAEffect;
    ID:Integer;
    procedure Reinitialize;
    procedure LoadInformations;
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

procedure TFormEditor.ToggleBoxInfoClick(Sender:TObject);
begin
  if not FEditorOpening then ToggleBoxInfo.Checked:=True;
  if FEditorOpening and ToggleBoxInfo.Checked then
  begin
    FEditorOpening:=False;
    ToggleBoxPlug.Checked:=not ToggleBoxInfo.Checked;
    CloseEditor;
    self.Width:=FPluginfo.Width;
    self.Height:=PanelTools.Height+FPluginfo.Height;
    FPluginfo.Show;
  end;
end;

procedure TFormEditor.ToggleBoxPlugClick(Sender:TObject);
begin
  if FEditorOpening then ToggleBoxPlug.Checked:=True;
  if not FEditorOpening and ToggleBoxPlug.Checked then
  begin
    FEditorOpening:=True;
    ToggleBoxInfo.Checked:=not ToggleBoxPlug.Checked;
    FPluginfo.Hide;
    OpenEditor;
    FEditorOpening:=True;
  end;
end;

procedure TFormEditor.OpenEditor;
var
  Rect:PERect;
begin
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditOpen,0,0,FromIntPtr(PanelMain.Handle),0);
  Rect:=nil;
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditGetRect,0,0,@Rect,0);
  if Assigned(Rect) then
  begin
    self.Width:=Rect^.Right-Rect^.Left;
    self.Height:=Rect^.Bottom-Rect^.Top+PanelTools.Height;
  end;
end;

procedure TFormEditor.CloseEditor;
begin
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effEditClose,0,0,nil,0);
end;

procedure TFormEditor.Reinitialize;
var
  i:integer;
begin
  FEditorOpening:=True;
  AEffect:=nil;
  FPluginfo.Visible:=False;
  ToggleBoxPlug.Checked:=True;
  ToggleBoxInfo.Checked:=False;
  for i:=0 to FPluginfo.ListViewInfo.Items.Count-1 do
    FPluginfo.ListViewInfo.Items.Item[i].SubItems.Clear;
end;

procedure TFormEditor.LoadInformations;
var
  buf:array[0..255] of char;
begin
  if IsConsole then WriteLn('Loading informations, ID: ',ID);
  buf[0]:=#0;
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effGetEffectName,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[0].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effGetVendorString,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[1].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
  if Assigned(AEffect) then AEffect^.Dispatcher(AEffect,effGetProductString,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[2].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
end;

procedure TFormEditor.FormClose(Sender:TObject; var CloseAction:TCloseAction);
begin
  if IsConsole then Writeln('Editor Close, ID: ',ID);
  CloseAction:=caNone;
  FormPlugManager.UncheckBox(ID);
end;

procedure TFormEditor.FormCreate(Sender:TObject);
begin
  FPluginfo:=TFramePluginfo.Create(self);
  FPluginfo.Parent:=PanelMain;
  FPluginfo.Visible:=False;
  FEditorOpening:=True;
end;

procedure TFormEditor.FormHide(Sender:TObject);
begin
  if IsConsole then Writeln('Editor Hide, ID: ',ID);
  if FEditorOpening then CloseEditor;
end;

procedure TFormEditor.FormShow(Sender:TObject);
begin
  if IsConsole then Writeln('Editor Show, ID: ',ID);
  if FEditorOpening then OpenEditor;
end;

end.

