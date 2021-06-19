{-------------------------------------------------------------------------------
// This unit is part of vst24pas
// Unit name   : vst24pas.gui
// Description : Extension for editor class (use lcl or vcl)
// Created by  : PeaZomboss, 2021/5
-------------------------------------------------------------------------------}
unit vst24pas.gui;

{$I vst24pas.inc}

interface

uses
  Forms, vst24pas.Core, vst24pas.Base;

type

  { TGuiEditor }

  TGuiEditor = class(TVstEditor)
  protected
    FGui:  TForm;
    FRect: TERect;
  public
    constructor Create(Gui: TForm; Plugin: TVstPlugin = nil);
    destructor Destroy; override;
    function GetRect(rect: PPERect): boolean; override;
    function Open(ptr: Pointer): boolean; override;
    procedure Close; override;
    property Gui: TForm read FGui;
  end;

implementation

uses
  SysUtils, Controls;

{ TGuiEditor }

constructor TGuiEditor.Create(Gui: TForm; Plugin: TVstPlugin);
begin
  if not Assigned(Gui) then
    raise Exception.Create('TGuiEditor.Create : Invalid Gui');
  inherited Create(Plugin);
  FGui := Gui;
  FGui.BorderStyle := bsNone;
  FGui.Left := 0;
  FGui.Top := 0;
end;

destructor TGuiEditor.Destroy;
begin
  FreeAndNil(FGui);
  inherited Destroy;
end;

function TGuiEditor.GetRect(rect: PPERect): boolean;
begin
  FRect.Left := 0;
  FRect.Top := 0;
  FRect.Right := FGui.Width;
  FRect.Bottom := FGui.Height;
  rect^  := @FRect;
  Result := True;
end;

function TGuiEditor.Open(ptr: Pointer): boolean;
begin
  FGui.ParentWindow := ToIntPtr(ptr);
  FGui.Show;
  Result := True;
end;

procedure TGuiEditor.Close;
begin
  FGui.Hide;
  inherited Close;
end;

end.
