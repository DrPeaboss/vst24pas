{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugui
// Description : Gui for vst2 plugin
// Created by  : PeaZomboss, 2021/07

// Use lcl or vcl for gui plugin development
// Usually use this unit to make plugin with gui
-------------------------------------------------------------------------------}

unit vst2plugui;

{$I vst2def.inc}

interface

uses
  vst2intf,vst2plugbas,vst2plugin,forms;

type
  { TVEditor }

  TVEditor = class(TVEditorBase,IVEditor)
  private
    FPlugin:TVPlugin;
    FGui:TForm;
    FRect:TERect;
    FOpening:Boolean;
    FOnIdle:TObjProc;
  protected
    function IsOpen:Boolean;
    procedure EditBegin(index:integer);
    procedure EditEnd(index:integer);
    procedure SetIdle(IdleProc:TObjProc);
    procedure SetGui(FrmClass:TClass);
    function GetGui:TObject;
  public
    constructor Create(Plugin:TVPlugin;FrmClass:TClass=nil);
    destructor Destroy;override;
    procedure Open(ParentHandle:Pointer);override;
    procedure Close;override;
    procedure GetRect(const Rect:PPERect);override;
    procedure Idle;override;
    //property Plugin:TVPlugin read FPlugin;
  end;

  { TVGuiPlugin }

  TVGuiPlugin = class(TVPlugin)
  public
    constructor Create(AHost:THostCallback);override;
    destructor Destroy;override;
  end;

function NewEditor(Plugin:TVPlugin;FrmClass:TClass=nil):IVEditor;

implementation

uses
  controls;

{ TVEditor }

constructor TVEditor.Create(Plugin:TVPlugin;FrmClass:TClass);
begin
  FPlugin:=Plugin;
  FOpening:=False;
  FGui:=nil;
  SetGui(FrmClass);
end;

destructor TVEditor.Destroy;
begin
  if Assigned(FGui) then
    FGui.Free;
  inherited destroy;
end;

procedure TVEditor.SetGui(FrmClass:TClass);
begin
  if not Assigned(FGui) and FrmClass.InheritsFrom(TForm) then
  begin
    FGui:=TForm(FrmClass.newinstance).Create(nil);
    //FGui.Parent:=nil;
    FGui.SetBounds(0,0,FGui.Width,FGui.Height);
    FGui.BorderStyle:=bsNone;
    FPlugin.Base.SetFlag(effFlagsHasEditor);
    {$ifdef debug}dbgln('Set gui successfully, class name: %s',[FrmClass.ClassName]);{$endif}
  end;
end;

procedure TVEditor.Open(ParentHandle:Pointer);
begin
  FGui.ParentWindow:=ToIntPtr(ParentHandle);
  FGui.Show;
  FPlugin.Base.CallHost(amSizeWindow,FGui.Width,FGui.Height);
  FOpening:=True;
end;

procedure TVEditor.Close;
begin
  FGui.Hide;
  FGui.ParentWindow:=0;
  FOpening:=False;
end;

procedure TVEditor.GetRect(const Rect:PPERect);
begin
  //{$ifdef debug}dbgln('Get rect, w: %d, h: %d',[FGui.Width,FGui.Height]);{$endif}
  FRect.Left:=0;
  FRect.Top:=0;
  FRect.Right:=FGui.Width;
  FRect.Bottom:=FGui.Height;
  Rect^:=@FRect;
end;

procedure TVEditor.Idle;
begin
  if Assigned(FOnIdle) then FOnIdle;
end;

function TVEditor.IsOpen:Boolean;
begin
  Result:=FOpening;
end;

procedure TVEditor.EditBegin(index:integer);
begin
  {$ifdef debug}dbgln('Called EditBegin, index: %d',[index]);{$endif}
  FPlugin.Base.CallHost(amBeginEdit,index);
end;

procedure TVEditor.EditEnd(index:integer);
begin
  {$ifdef debug}dbgln('Called EditEnd, index: %d',[index]);{$endif}
  FPlugin.Base.CallHost(amEndEdit,index);
end;

procedure TVEditor.SetIdle(IdleProc:TObjProc);
begin
  FOnIdle:=IdleProc;
end;

function TVEditor.GetGui:TObject;
begin
  Result:=FGui;
end;

{ TVGuiPlugin }

constructor TVGuiPlugin.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  SetEditor(TVEditor.Create(self));
end;

destructor TVGuiPlugin.Destroy;
begin
  inherited Destroy;
end;

function NewEditor(Plugin:TVPlugin;FrmClass:TClass):IVEditor;
begin
  Result:=TVEditor.Create(Plugin,FrmClass);
end;

end.

