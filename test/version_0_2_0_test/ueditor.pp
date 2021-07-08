unit ueditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls, Menus,
  vst2interfaces;

type

  { TFormMain }

  TFormMain = class(TForm)
    ListView1:    TListView;
    MemoLog:      TMemo;
    MenuItemClear: TMenuItem;
    PopupMenuMemo: TPopupMenu;
    Timer:        TTimer;
    ToggleBoxPause: TToggleBox;
    TrackBarGain: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure MenuItemClearClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarGainChange(Sender: TObject);
  private
    FAutoChanging: boolean;
  public
    opTimes: array[0..Ord(effGetNumMidiOutputChannels)] of integer;
  end;

implementation

uses
  umain, typinfo;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.TrackBarGainChange(Sender: TObject);
begin
  if not FAutoChanging then
    gPlugin.Parameters[0] := TrackBarGain.Position / 2000;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  FAutoChanging := True;
  TrackBarGain.Position := Round(2000 * gPlugin.Parameters[0]);
  FAutoChanging := False;
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

procedure TFormMain.MenuItemClearClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

end.

