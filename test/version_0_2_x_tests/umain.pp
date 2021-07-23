unit umain;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugin;

type

  { TMyPlugin }

  TMyPlugin = class(TVPlugin)
  protected
    procedure ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);override;
  public
    constructor Create(AHost:THostCallback);//override;
  end;

implementation

{ TMyPlugin }

constructor TMyPlugin.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  Base.SetUniqueID('PZn3');
  Param.AddParameter(0.5,'Gain','');
end;

procedure TMyPlugin.ProcessRep(const inputs,outputs:TBuffer32;SampleFrames:Int32);
var
  i:Integer;
begin
  for i:=0 to SampleFrames-1 do
  begin
    outputs[0,i]:=inputs[0,i]*Param[0];
    outputs[1,i]:=inputs[1,i]*Param[0];
  end;
end;

end.

