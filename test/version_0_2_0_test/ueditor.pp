unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,
  vst2interfaces;

type

  { TFormMain }

  TFormMain = class(TForm)
    LabelCnt: TLabel;
    LabelIdleCount: TLabel;
    ListView1:      TListView;
    MemoLog:        TMemo;
    MenuItemBlockEditIdle: TMenuItem;
    MenuItemClear:  TMenuItem;
    PopupMenuMemo:  TPopupMenu;
    Timer:          TTimer;
    ToggleBoxPause: TToggleBox;
    TrackBarGain:   TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemBlockEditIdleClick(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarGainChange(Sender: TObject);
  private
  public
    AutoChanging: boolean;
    BlockEditIdle: boolean;
    opTimes: array[0..Ord(effGetNumMidiOutputChannels)] of integer;
  end;

implementation

uses
  umain, typinfo;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TrackBarGainChange(Sender: TObject);
begin
  if not AutoChanging then
    gPlugin.Parameters[0] := TrackBarGain.Position / 2000;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  AutoChanging := True;
  TrackBarGain.Position := Round(2000 * gPlugin.Parameters[0]);
  LabelCnt.Caption := IntToStr(gPlugin.Process32Count);
  AutoChanging := False;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  i: integer;
begin
  // We should add items every time form show because the items will be clear when form hide
  for i := 0 to Ord(effGetNumMidiOutputChannels) do
    with ListView1.Items.Add do
    begin
      Caption := GetEnumName(typeinfo(TAEffectOpcodes), i);
      SubItems.Add(IntToStr(opTimes[i]));
    end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  BlockEditIdle := PopupMenuMemo.Items[1].Checked;
end;

procedure TFormMain.MenuItemBlockEditIdleClick(Sender: TObject);
begin
  BlockEditIdle := PopupMenuMemo.Items[1].Checked;
end;

procedure TFormMain.MenuItemClearClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

end.

