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
    RadioButtonInfo:TRadioButton;
    RadioButtonPlug:TRadioButton;
    Timer:TTimer;
    procedure FormClose(Sender:TObject; var CloseAction:TCloseAction);
    procedure FormCreate(Sender:TObject);
    procedure FormHide(Sender:TObject);
    procedure FormShow(Sender:TObject);
    procedure RadioButtonInfoClick(Sender:TObject);
    procedure RadioButtonPlugClick(Sender:TObject);
    procedure TimerTimer(Sender:TObject);
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
  RadioButtonPlug.Checked:=True;
  for i:=0 to FPluginfo.ListViewInfo.Items.Count-1 do
    FPluginfo.ListViewInfo.Items.Item[i].SubItems.Clear;
end;

procedure TFormEditor.LoadInformations;

  function UniqueID2String(value:int32):string;
  var
    chrs:array[0..3] of char absolute value;
  begin
    Result:=chrs[3]+chrs[2]+chrs[1]+chrs[0];
  end;

var
  buf:array[0..255] of char;
begin
  if IsConsole then WriteLn('Loading informations, ID: ',ID);
  if not Assigned(AEffect) then Exit;
  buf[0]:=#0;
  AEffect^.Dispatcher(AEffect,effGetEffectName,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[0].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
  AEffect^.Dispatcher(AEffect,effGetVendorString,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[1].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
  AEffect^.Dispatcher(AEffect,effGetProductString,0,0,@buf,0);
  FPluginfo.ListViewInfo.Items.Item[2].SubItems.Add(StrPas(buf));
  buf[0]:=#0;
  FPluginfo.ListViewInfo.Items.Item[3].SubItems.Add(IntToStr(AEffect^.NumParams));
  FPluginfo.ListViewInfo.Items.Item[4].SubItems.Add(IntToStr(AEffect^.NumPrograms));
  FPluginfo.ListViewInfo.Items.Item[5].SubItems.Add(IntToStr(AEffect^.NumInputs));
  FPluginfo.ListViewInfo.Items.Item[6].SubItems.Add(IntToStr(AEffect^.NumOutputs));
  FPluginfo.ListViewInfo.Items.Item[7].SubItems.Add(UniqueID2String(AEffect^.UniqueID));
end;

procedure TFormEditor.FormClose(Sender:TObject; var CloseAction:TCloseAction);
begin
  if IsConsole then Writeln('Editor Close, ID: ',ID);
  CloseAction:=caNone;
  FormPlugManager.UncheckBox(ID);
end;

procedure TFormEditor.FormCreate(Sender:TObject);
begin
  FPluginfo:=TFramePluginfo.Create(PanelMain);
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

procedure TFormEditor.RadioButtonInfoClick(Sender:TObject);
begin
  if FEditorOpening then
  begin
    FEditorOpening:=False;
    CloseEditor;
    FPluginfo.Show;
    self.Width:=FPluginfo.Width;
    self.Height:=PanelTools.Height+FPluginfo.Height;
  end;
end;

procedure TFormEditor.RadioButtonPlugClick(Sender:TObject);
begin
  if not FEditorOpening then
  begin
    FEditorOpening:=True;
    FPluginfo.Hide;
    OpenEditor;
  end;
end;

end.

