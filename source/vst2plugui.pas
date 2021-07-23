{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst2plugui
// Description : Gui for vst2 plugin
// Created by  : PeaZomboss, 2021/07

// Use lcl or vcl for gui plugin development
// Usually use this unit to make plugin with gui
-------------------------------------------------------------------------------}

unit vst2plugui;

{$I vcompiler.inc}

interface

uses
  vst2intf,vst2plugbas,vst2plugin,forms;

const
  iidIVEditor:TGuid='{C34A39C2-8410-41BC-97E0-F473300CF1C5}';
  iidIVGuiEditor:TGuid='{1AE4F9F1-EBE5-4C4B-8088-C2D3D91ACFDF}';

type
  IVEditor = interface
    ['{C34A39C2-8410-41BC-97E0-F473300CF1C5}']
    function GetParam:IVParam;
    function GetPreset:IVPreset;
    property Param:IVParam read GetParam;
    property Preset:IVPreset read GetPreset;
  end;

  IVGuiEditor = interface(IVEditor)
    ['{1AE4F9F1-EBE5-4C4B-8088-C2D3D91ACFDF}']
    function IsOpen:Boolean;
    procedure EditBegin(index:integer);
    procedure EditEnd(index:integer);
    procedure SetIdle(IdleProc:TObjProc);
    procedure SetGui(GuiClass:TFormClass);
    function GetGui:TForm;
    property Gui:TForm read GetGui;
  end;

  { TVEditor }

  TVEditor = class(TInterfacedObject,IVEditor)
  private
    FBase:IVPlugBase;
  protected
    function GetParam:IVParam;
    function GetPreset:IVPreset;
  public
    constructor Create(ABase:IVPlugBase);
    destructor destroy;override;
    property Base:IVPlugBase read FBase;
  end;

  { TVGuiEditor }

  TVGuiEditor = class(TVEditor,IVGuiEditor)
  private
    FGui:TForm;
    FRect:TERect;
    FOpening:Boolean;
    FOnIdle:TObjProc;
  protected
    function IsOpen:Boolean;
    procedure EditBegin(index:integer);
    procedure EditEnd(index:integer);
    procedure SetIdle(IdleProc:TObjProc);
    function GetGui:TForm;
  public
    constructor Create(ABase:IVPlugBase);
    destructor destroy;override;
    procedure SetGui(GuiClass:TFormClass);
    procedure Open(const ParentHandle:PtrUInt); // THandle is longint, we use PtrUInt
    procedure Close;
    procedure GetRect(const Rect:PPERect);
    procedure Idle;
  end;

  { TVGuiPlugin }

  TVGuiPlugin = class(TVPlugin,IVGuiEditor)
  private
    FGuiEditor:TVGuiEditor;
    FEditor:IVGuiEditor;
  protected
    function Dispatcher(opcode:TAEOpcodes;index:Int32;const value:IntPtr;const ptr:Pointer;opt:Single):IntPtr;override;
  public
    constructor Create(AHost:THostCallback);//override;
    destructor destroy;override;
    property Editor:IVGuiEditor read FEditor implements IVGuiEditor;
  end;

implementation

uses
  controls{$ifdef debug},sysutils{$endif};

{ TVEditor }

constructor TVEditor.Create(ABase:IVPlugBase);
begin
  //{$ifdef debug}dbgln('TVEditor create');{$endif}
  FBase:=ABase;
end;

destructor TVEditor.destroy;
begin
  //{$ifdef debug}dbgln('TVEditor destroy');{$endif}
  inherited destroy;
end;

function TVEditor.GetParam:IVParam;
begin
  {$ifdef debug}dbgln('Called GetParam');{$endif}
  FBase.QueryInterface(iidIVParam,Result);
  FBase._Release;
end;

function TVEditor.GetPreset:IVPreset;
begin
  {$ifdef debug}dbgln('Called GetPreset');{$endif}
  FBase.QueryInterface(iidIVPreset,Result);
  FBase._Release;
end;

{ TVGuiEditor }

constructor TVGuiEditor.Create(ABase:IVPlugBase);
begin
  inherited Create(ABase);
  FOpening:=False;
  FGui:=nil;
  //{$ifdef debug}dbgln('TVGuiEditor create');{$endif}
end;

procedure TVGuiEditor.Close;
begin
  //{$ifdef debug}dbgln('Called editor close');{$endif}
  FGui.Hide;
  FGui.ParentWindow:=0;
  FOpening:=False;
end;

destructor TVGuiEditor.destroy;
begin
  //{$ifdef debug}dbgln('TVGuiEditor destroy');{$endif}
  FGui.Free;
  inherited destroy;
end;

procedure TVGuiEditor.EditBegin(index:integer);
begin
  {$ifdef debug}dbgln('Called EditBegin, index: %d',[index]);{$endif}
  Base.CallHost(amBeginEdit,index);
end;

procedure TVGuiEditor.EditEnd(index:integer);
begin
  {$ifdef debug}dbgln('Called EditEnd, index: %d',[index]);{$endif}
  Base.CallHost(amEndEdit,index);
end;

function TVGuiEditor.GetGui:TForm;
begin
  Result:=FGui;
end;

procedure TVGuiEditor.GetRect(const Rect:PPERect);
begin
  {$ifdef debug}dbgln('Get rect, w: %d, h: %d',[FGui.Width,FGui.Height]);{$endif}
  FRect.Left:=0;
  FRect.Top:=0;
  FRect.Right:=FGui.Width;
  FRect.Bottom:=FGui.Height;
  Rect^:=@FRect;
end;

procedure TVGuiEditor.Idle;
begin
  if Assigned(FOnIdle) then FOnIdle;
end;

function TVGuiEditor.IsOpen:Boolean;
begin
  Result:=FOpening;
end;

procedure TVGuiEditor.Open(const ParentHandle:PtrUInt);
begin
  {$ifdef debug}dbgln('Called editor open, handle: %X',[ParentHandle]);{$endif}
  FGui.ParentWindow:=ParentHandle;
  FGui.Show;
  Base.CallHost(amSizeWindow,FGui.Width,FGui.Height);
  FOpening:=True;
end;

procedure TVGuiEditor.SetGui(GuiClass:TFormClass);
begin
  if not Assigned(FGui) then
  begin
    FGui:=GuiClass.Create(nil);
    //FGui.Parent:=nil;
    FGui.Left:=0;
    FGui.Top:=0;
    FGui.BorderStyle:=bsNone;
    Include(Base.Effect.Flags,effFlagsHasEditor);
    {$ifdef debug}dbgln('Set gui successfully, class name: %s',[GuiClass.ClassName]);{$endif}
  end;
end;

procedure TVGuiEditor.SetIdle(IdleProc:TObjProc);
begin
  FOnIdle:=IdleProc;
end;

{ TVGuiPlugin }

constructor TVGuiPlugin.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  FGuiEditor:=TVGuiEditor.Create(Base);
  FGuiEditor.GetInterface(iidIVGuiEditor,FEditor);
  //{$ifdef debug}dbgln('TVGuiPlugin create');{$endif}
end;

destructor TVGuiPlugin.destroy;
begin
  //{$ifdef debug}dbgln('TVGuiPlugin destroy');{$endif}
  inherited destroy;
end;

function TVGuiPlugin.Dispatcher(opcode:TAEOpcodes;index:Int32;const value:IntPtr;const ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=0;
  case opcode of
    effEditOpen:FGuiEditor.Open(ToIntPtr(ptr));
    effEditClose:FGuiEditor.Close;
    effEditIdle:FGuiEditor.Idle;
    effEditGetRect:FGuiEditor.GetRect(ptr);
    else Result:=inherited Dispatcher(opcode,index,value,ptr,opt);
  end;
end;

end.

