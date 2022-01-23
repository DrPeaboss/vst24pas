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
    ButtonNextPreset:TButton;
    ButtonPrevPreset:TButton;
    ButtonRandomPreset:TButton;
    ButtonRenamePreset:TButton;
    ButtonInsertPreset:TButton;
    LabelGain:TLabel;
    LabelPresetShow:TLabel;
    MemoLog:TMemo;
    TrackBarGain:TTrackBar;
    procedure ButtonAddPresetClick(Sender:TObject);
    procedure ButtonDeletePresetClick(Sender:TObject);
    procedure ButtonInitPresetClick(Sender:TObject);
    procedure ButtonNextPresetClick(Sender:TObject);
    procedure ButtonPrevPresetClick(Sender:TObject);
    procedure ButtonRandomPresetClick(Sender:TObject);
    procedure ButtonRenamePresetClick(Sender:TObject);
    procedure ButtonInsertPresetClick(Sender:TObject);
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
    procedure UpdatePreset;
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
  UpdatePreset;
end;

procedure TFormTest2.ButtonNextPresetClick(Sender:TObject);
begin
  Plugin.Preset.NextPreset;
  UpdatePreset;
end;

procedure TFormTest2.ButtonPrevPresetClick(Sender:TObject);
begin
  Plugin.Preset.PrevPreset;
  UpdatePreset;
end;

procedure TFormTest2.ButtonRandomPresetClick(Sender:TObject);
begin
  Plugin.Preset.RandomPreset;
  UpdatePreset;
end;

procedure TFormTest2.ButtonRenamePresetClick(Sender:TObject);
begin
  Plugin.Preset.RenamePreset(TimeToStr(Now));
  UpdatePreset;
end;

procedure TFormTest2.ButtonInsertPresetClick(Sender:TObject);
begin
  Plugin.Preset.InsertPreset;
  UpdatePreset;
end;

procedure TFormTest2.ButtonDeletePresetClick(Sender:TObject);
begin
  Plugin.Preset.DeletePreset;
  UpdatePreset;
end;

procedure TFormTest2.ButtonAddPresetClick(Sender:TObject);
var
  e:Extended;
begin
  Randomize;
  e:=Random;
  Plugin.Preset.AddPreset(Format('Preset %.2f',[e]),[e]);
  UpdatePreset;
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

procedure TFormTest2.UpdatePreset;
begin
  LabelPresetShow.Caption:=Format('Cur:%d, Num:%d, Name:%s',
    [Plugin.Preset.GetCurPreset,Plugin.Preset.GetPresetNum,Plugin.Preset.GetPresetName]);
end;

end.

