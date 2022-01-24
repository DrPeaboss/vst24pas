unit v02xtest2editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,ComCtrls,StdCtrls,
  v02xtest2main;

type

  { TFormTest2 }

  TFormTest2 = class(TForm)
    ButtonAddPreset:TButton;
    ButtonDeletePreset:TButton;
    ButtonInitPreset:TButton;
    ButtonLoadFromFile:TButton;
    ButtonNextPreset:TButton;
    ButtonPrevPreset:TButton;
    ButtonRandomPreset:TButton;
    ButtonRenamePreset:TButton;
    ButtonInsertPreset:TButton;
    ButtonSaveToFile:TButton;
    ComboBoxPresets:TComboBox;
    LabelGain:TLabel;
    LabelPresetShow:TLabel;
    MemoLog:TMemo;
    OpenDialogFxp:TOpenDialog;
    SaveDialogFxp:TSaveDialog;
    TrackBarGain:TTrackBar;
    procedure ButtonAddPresetClick(Sender:TObject);
    procedure ButtonDeletePresetClick(Sender:TObject);
    procedure ButtonInitPresetClick(Sender:TObject);
    procedure ButtonLoadFromFileClick(Sender:TObject);
    procedure ButtonNextPresetClick(Sender:TObject);
    procedure ButtonPrevPresetClick(Sender:TObject);
    procedure ButtonRandomPresetClick(Sender:TObject);
    procedure ButtonRenamePresetClick(Sender:TObject);
    procedure ButtonInsertPresetClick(Sender:TObject);
    procedure ButtonSaveToFileClick(Sender:TObject);
    procedure ComboBoxPresetsChange(Sender:TObject);
    procedure FormCreate(Sender:TObject);
    procedure TrackBarGainChange(Sender:TObject);
    procedure TrackBarGainMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
    procedure TrackBarGainMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
  private
    FChanging:Boolean;
  public
    Plugin:TTestPlugin2;
    procedure Idle;
    procedure logln(const log:string);overload;
    procedure logln(const fmt:string;const args:array of const);overload;
    procedure UpdateDisplay;
    procedure UpdatePresetItems;
  end;

implementation

uses
  vst2utils;

{$R *.lfm}

{ TFormTest2 }

procedure TFormTest2.FormCreate(Sender:TObject);
begin
  FChanging:=False;
  LabelGain.Caption:=Format('Gain: %.2fdB  %.2f',[VstAmp2dB(1.0),0.5]);
  // Cannot access Plugin here !
  // Plugin.Editor.SetIdle(@Idle); //< It's not allowed
  LabelPresetShow.Caption:='';
end;

procedure TFormTest2.ButtonInitPresetClick(Sender:TObject);
begin
  Plugin.Preset.InitPreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonLoadFromFileClick(Sender:TObject);
begin
  OpenDialogFxp.InitialDir:=Plugin.Host.GetDirectory;
  if OpenDialogFxp.Execute then
  begin
    if Plugin.Preset.LoadFromFile(OpenDialogFxp.FileName) then
      ShowMessage('Load OK')
    else
      ShowMessage('Load Error');
  end;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonNextPresetClick(Sender:TObject);
begin
  ComboBoxPresets.ItemIndex:=Plugin.Preset.NextPreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonPrevPresetClick(Sender:TObject);
begin
  ComboBoxPresets.ItemIndex:=Plugin.Preset.PrevPreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonRandomPresetClick(Sender:TObject);
begin
  Plugin.Preset.RandomPreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonRenamePresetClick(Sender:TObject);
begin
  Plugin.Preset.RenamePreset(TimeToStr(Now));
  UpdateDisplay;
end;

procedure TFormTest2.ButtonInsertPresetClick(Sender:TObject);
begin
  Plugin.Preset.InsertPreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonSaveToFileClick(Sender:TObject);
begin
  SaveDialogFxp.InitialDir:=Plugin.Host.GetDirectory;
  if SaveDialogFxp.Execute then
  begin
    if Plugin.Preset.SaveToFile(SaveDialogFxp.FileName) then
      ShowMessage('Save OK')
    else
      ShowMessage('Save Error');
  end;
end;

procedure TFormTest2.ComboBoxPresetsChange(Sender:TObject);
begin
  Plugin.Preset.SetPreset(ComboBoxPresets.ItemIndex);
  UpdateDisplay;
end;

procedure TFormTest2.ButtonDeletePresetClick(Sender:TObject);
begin
  Plugin.Preset.DeletePreset;
  UpdateDisplay;
end;

procedure TFormTest2.ButtonAddPresetClick(Sender:TObject);
var
  e:Extended;
begin
  Randomize;
  e:=Random;
  Plugin.Preset.AddPreset(Format('Preset %.2f',[e]),[e]);
  UpdateDisplay;
end;

procedure TFormTest2.Idle;
begin
  // Use FChanging to avoid calling SetParamAutomated
  FChanging:=True;
  TrackBarGain.Position:=Round(2000*Plugin.Param.Items[0]);
  FChanging:=False;
end;

procedure TFormTest2.logln(const log:string);
begin
  MemoLog.Lines.Add(TimeToStr(Now)+'> '+log);
end;

procedure TFormTest2.logln(const fmt:string;const args:array of const);
begin
  MemoLog.Lines.Add(TimeToStr(Now)+'> '+fmt,args);
end;

procedure TFormTest2.TrackBarGainChange(Sender:TObject);
var
  gain:single;
begin
  gain:=TrackBarGain.Position/2000;
  if not FChanging then
    Plugin.Param.SetParamAutomated(0,gain);
  LabelGain.Caption:=Format('Gain: %.2fdB  %.2f',[VstAmp2dB(gain*2),gain]);
end;

procedure TFormTest2.TrackBarGainMouseDown(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  // Optional, not mandatory
  Plugin.Editor.EditBegin(0);
end;

procedure TFormTest2.TrackBarGainMouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
  // Optional, not mandatory
  Plugin.Editor.EditEnd(0);
end;

procedure TFormTest2.UpdateDisplay;
begin
  LabelPresetShow.Caption:=Format('Cur:%d, Num:%d, Name:%s',
    [Plugin.Preset.GetCurPreset,Plugin.Preset.GetPresetNum,Plugin.Preset.GetPresetName]);
  UpdatePresetItems;
end;

procedure TFormTest2.UpdatePresetItems;
var
  strarr:array of AnsiString;
  astr:AnsiString;
  curpreset,presets:Integer;
begin
  if Plugin.Preset.NumberNameChanged then
  begin
    ComboBoxPresets.Items.Clear;
    strarr := Plugin.Preset.GetPresetNameArray;
    if Length(strarr)>0 then
    begin
      for astr in strarr do
        ComboBoxPresets.Items.Add(astr);
      curpreset:=Plugin.Preset.GetCurPreset;
      presets:=Plugin.Preset.GetPresetNum;
      if curpreset<presets then
        ComboBoxPresets.ItemIndex:=curpreset;
      end;
  end;
end;

end.

