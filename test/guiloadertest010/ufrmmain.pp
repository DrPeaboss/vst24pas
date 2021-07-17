unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,Menus,
  uloader,ueditor;

type

  { TFormPlugManager }

  TFormPlugManager = class(TForm)
    ButtonUnloadAll:TButton;
    MenuItemCloneTo:TMenuItem;
    MenuItemUnload:TMenuItem;
    OpenDialogLoad: TOpenDialog;
    PanelPlugs: TPanel;
    PopupMenuPlugin:TPopupMenu;
    procedure ButtonUnloadAllClick(Sender:TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemUnloadClick(Sender:TObject);
    procedure PopupMenuPluginPopup(Sender:TObject);
  private
    FToggleBoxs: array[0..9] of TToggleBox;
    FButtons:array[0..9] of TButton;
    FEditors:array[0..9] of TFormEditor;
    FPlugManager:TPlugManager;
    procedure LoadPlugin(PlugName:string;ID:Int32);
    procedure UnloadPlugin(ID:Int32);
    procedure BtnLoadClick(Sender: TObject);
    procedure BoxSlotClick(Sender: TObject);
    procedure MenuCloneToClick(Sender:TObject);
    procedure ShowEditor(ID:Int32);
  public
    procedure UncheckBox(ID:Int32);
    procedure ResizeEditor(ID,W,H:Integer);
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
  item: TMenuItem;
  editor :TFormEditor;
begin
  if IsConsole then Writeln('Init plug manager ...');
  FPlugManager:=TPlugManager.Create(self);
  for i := 0 to 9 do
  begin
    item:=TMenuItem.Create(PopupMenuPlugin.Items.Items[1]);
    item.Name:='Clone'+Chr(i+48);
    item.Caption:='Slot '+Chr(i+48);
    item.ImageIndex:=-1; { TODO : Test For Linux, fix bugs }
    item.OnClick:=@MenuCloneToClick;
    PopupMenuPlugin.Items.Items[1].Add(item);
    editor:=TFormEditor.Create(Application);
    editor.Left:=30*i;
    editor.Top:=15*i;
    editor.ID:=i;
    FEditors[i]:=editor;
    box:=TToggleBox.Create(PanelPlugs);
    box.Parent:=PanelPlugs;
    box.Left := 50;
    box.Width := 200;
    box.Height := 35;
    box.Top := 35*i+20;
    box.Enabled := False;
    box.PopupMenu:=PopupMenuPlugin;
    box.Name := 'Slot'+Chr(i+48);
    box.Caption := 'Slot '+Chr(i+48);
    box.OnClick:=@BoxSlotClick;
    FToggleBoxs[i]:=box;
    btn:=TButton.Create(PanelPlugs);
    btn.Parent:=PanelPlugs;
    btn.Left := 10;
    btn.Width := 35;
    btn.Height := 35;
    btn.Top := 35*i+20;
    btn.Name := 'Load'+Chr(i+48);
    btn.Caption := '...';
    btn.OnClick := @BtnLoadClick;
    FButtons[i]:=btn;
  end;
end;

procedure TFormPlugManager.ButtonUnloadAllClick(Sender:TObject);
var
  i,count:Integer;
begin
  if IsConsole then Writeln('Unload all plugins clicked');
  count:=0;
  for i:=0 to 9 do
    if FPlugManager.IsPlugLoaded(i) then
    begin
      UnloadPlugin(i);
      inc(count);
    end;
  if IsConsole then Writeln('Unloaded ',count,' plugins');
end;

procedure TFormPlugManager.MenuItemUnloadClick(Sender:TObject);
var
  ID:integer;
begin
  ID:=ord(PopupMenuPlugin.PopupComponent.Name[5])-48;
  if IsConsole then Writeln('Unload clicked, ID: ',ID);
  UnloadPlugin(ID);
end;

procedure TFormPlugManager.PopupMenuPluginPopup(Sender:TObject);
var
  i:integer;
begin
  for i:=0 to 9 do
  begin
    if FPlugManager.IsPlugLoaded(i) then
      PopupMenuPlugin.Items.Items[1].Items[i].Enabled:=False
    else
      PopupMenuPlugin.Items.Items[1].Items[i].Enabled:=True;
  end;
end;

procedure TFormPlugManager.LoadPlugin(PlugName:string; ID:Int32);
var
  buf1:array[0..63] of char;
  buf2:array[0..63] of char;
begin
  if FPlugManager.TryLoadPlugin(PlugName,ID) then
  begin
    buf1[0]:=#0;
    buf2[0]:=#0;
    FToggleBoxs[ID].Enabled := True;
    FPlugManager.InitPlugin(ID);
    FButtons[ID].Enabled:=False;
    FEditors[ID].AEffect:=FPlugManager.AEffect[ID];
    FPlugManager.Dispatcher(ID,effGetEffectName,0,0,@buf1,0);
    FPlugManager.Dispatcher(ID,effGetVendorString,0,0,@buf2,0);
    FToggleBoxs[ID].Caption:=StrPas(@buf1);
    FEditors[ID].Caption:=StrPas(@buf1)+' ('+StrPas(@buf2)+')';
    FToggleBoxs[ID].Checked:=True;
    FEditors[ID].LoadInformations;
  end;
end;

procedure TFormPlugManager.UnloadPlugin(ID:Int32);
begin
  if FEditors[ID].Showing then FEditors[ID].Hide;
  if FPlugManager.TryUnLoad(ID) then
  begin
    FToggleBoxs[ID].Caption:='Slot '+Chr(ID+48);
    FButtons[ID].Enabled:=True;
    FToggleBoxs[ID].Checked:=False;
    FToggleBoxs[ID].Enabled:=False;
    FEditors[ID].Reinitialize;
  end;
end;

procedure TFormPlugManager.BtnLoadClick(Sender: TObject);
var
  ID:integer;
begin
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
    LoadPlugin(OpenDialogLoad.FileName,ID);
  end;
end;

procedure TFormPlugManager.BoxSlotClick(Sender:TObject);
var
  ID:integer;
begin
  ID:=ord((sender as TToggleBox).Name[5])-48;
  if IsConsole then Writeln('Called BoxSlotClick, ID: ',ID);
  if FPlugManager.IsPlugLoaded(ID) then
    ShowEditor(ID);
end;

procedure TFormPlugManager.MenuCloneToClick(Sender:TObject);
var
  ToID,FromID:Integer;
begin
  FromID:=ord(PopupMenuPlugin.PopupComponent.Name[5])-48;
  ToID:=ord((Sender as TMenuItem).Name[6])-48;
  if IsConsole then Writeln('Clone plugin from ID: ',FromID,' to ID: ',ToID);
  LoadPlugin(FPlugManager.PlugFileName[FromID],ToID);
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

procedure TFormPlugManager.ResizeEditor(ID,W,H:Integer);
begin
  FEditors[ID].Width:=W;
  FEditors[ID].Height:=H+FEditors[ID].PanelTools.Height;
end;

end.

