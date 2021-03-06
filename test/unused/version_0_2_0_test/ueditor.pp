unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,
  vst2interfaces,vst2pluginbaseold,umain;

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
    procedure MenuItemBlockEditIdleClick(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarGainChange(Sender: TObject);
  private
  public
    Plugin:TMyPlugin;
    AutoChanging: boolean;
    BlockEditIdle: boolean;
    opTimes: array[0..Ord(effGetNumMidiOutputChannels)] of integer;
  end;

implementation

uses
  typinfo;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TrackBarGainChange(Sender: TObject);
begin
  if not AutoChanging then
    Plugin.Parameters[0] := TrackBarGain.Position / 2000;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  AutoChanging := True;
  TrackBarGain.Position := Round(2000 * Plugin.Parameters[0]);
  LabelCnt.Caption := IntToStr(Plugin.Process32Count);
  AutoChanging := False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  BlockEditIdle := PopupMenuMemo.Items[1].Checked;
  for i := 0 to Ord(effGetNumMidiOutputChannels) do
    with ListView1.Items.Add do
    begin
      Caption := GetEnumName(typeinfo(TAEffectOpcodes), i);
      SubItems.Add(IntToStr(opTimes[i]));
    end;
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

