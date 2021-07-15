unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,Menus,
  uloader,ueditor;

type

  { TFormPlugManager }

  TFormPlugManager = class(TForm)
    MenuItemUnload:TMenuItem;
    OpenDialogLoad: TOpenDialog;
    PanelPlugs: TPanel;
    PopupMenuPlugin:TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure MenuItemUnloadClick(Sender:TObject);
  private
    FToggleBoxs: array[0..9] of TToggleBox;
    FButtons:array[0..9] of TButton;
    FEditors:array[0..9] of TFormEditor;
    FPlugManager:TPlugManager;
  public
    procedure BtnLoadClick(Sender: TObject);
    procedure BoxPlugClick(Sender: TObject);
    procedure ShowEditor(ID:Int32);
    procedure UncheckBox(ID:Int32);
  end;

var
  FormPlugManager: TFormPlugManager;

implementation

uses
  vst24pas.core;

{$R *.lfm}

{ TFormPlugManager }

procedure TFormPlugManager.FormCreate(Sender: TObject);
var
  i: integer;
  box: TToggleBox;
  btn: TButton;
begin
  if IsConsole then Writeln('Init plug manager ...');
  FPlugManager:=TPlugManager.Create(self);
  for i := 0 to 9 do
  begin
    FEditors[i]:=TFormEditor.Create(Application);
    FEditors[i].ID:=i;
    box:=TToggleBox.Create(PanelPlugs);
    box.Parent:=PanelPlugs;
    box.Left := 50;
    box.Width := 200;
    box.Height := 35;
    box.Top := 35*i+20;
    box.Enabled := False;
    box.PopupMenu:=PopupMenuPlugin;
    box.Name := 'Plug'+IntToStr(i);
    box.Caption := 'Plug '+IntToStr(i);
    box.OnClick:=@BoxPlugClick;
    FToggleBoxs[i]:=box;
    btn:=TButton.Create(PanelPlugs);
    btn.Parent:=PanelPlugs;
    btn.Left := 10;
    btn.Width := 35;
    btn.Height := 35;
    btn.Top := 35*i+20;
    btn.Name := 'Load'+IntToStr(i);
    btn.Caption := '...';
    btn.OnClick := @BtnLoadClick;
    FButtons[i]:=btn;
  end;
end;

procedure TFormPlugManager.MenuItemUnloadClick(Sender:TObject);
var
  ID:integer;
begin
  ID:=ord(PopupMenuPlugin.PopupComponent.Name[5])-48;
  if IsConsole then Writeln('Unload clicked, ID: ',ID);
  if FPlugManager.TryUnLoad(ID) then;
  begin
    FButtons[ID].Enabled:=True;
    FToggleBoxs[ID].Checked:=False;
    FToggleBoxs[ID].Enabled:=False;
    FEditors[ID].AEffect:=nil;
    if FEditors[ID].Showing then FEditors[ID].Hide;
  end;
end;

procedure TFormPlugManager.BtnLoadClick(Sender: TObject);
var
  ID:integer;
  buf1:array[0..63] of char;
  buf2:array[0..63] of char;
begin
  buf1[0]:=#0;
  buf2[0]:=#0;
  ID:=ord((sender as TButton).Name[5])-48;
  if IsConsole then Writeln('Load clicked, ID: ',ID);
{$if defined(MSWINDOWS)}
  OpenDialogLoad.Filter := 'VST 2 Plugin|*.dll';
{$elseif defined(LINUX)}
  OpenDialogLoad.Filter := 'VST 2 Plugin|*.so';
{$endif}
  OpenDialogLoad.InitialDir:=GetCurrentDir;
  if OpenDialogLoad.Execute then
  begin
    if FPlugManager.TryLoadPlugin(OpenDialogLoad.FileName,ID) then
    begin
      FToggleBoxs[ID].Enabled := True;
      FPlugManager.InitPlugin(ID);
      FButtons[ID].Enabled:=False;
      FEditors[ID].AEffect:=FPlugManager.AEffect[ID];
      FPlugManager.Dispatcher(ID,effGetEffectName,0,0,@buf1,0);
      FPlugManager.Dispatcher(ID,effGetVendorString,0,0,@buf2,0);
      FEditors[ID].Caption:=StrPas(@buf1)+' ('+StrPas(@buf2)+')';
      FToggleBoxs[ID].Checked:=True;
    end;
  end;
end;

procedure TFormPlugManager.BoxPlugClick(Sender:TObject);
var
  ID:integer;
begin
  ID:=ord((sender as TToggleBox).Name[5])-48;
  if IsConsole then Writeln('Called BoxPlugClick, ID: ',ID);
  if FPlugManager.IsPlugLoaded(ID) then
    ShowEditor(ID);
end;

procedure TFormPlugManager.ShowEditor(ID:Int32);
begin
  if FToggleBoxs[ID].Checked then
  begin
    if FPlugManager.HasEditor(ID) then
      FEditors[ID].Show;
  end else begin
    FEditors[ID].Hide;
  end;
end;

procedure TFormPlugManager.UncheckBox(ID:Int32);
begin
  if IsConsole then Writeln('Uncheck called');
  FToggleBoxs[ID].Checked:=False;
end;

end.

