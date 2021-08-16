unit v02xtest3main;

{$mode ObjFPC}{$H+}

interface

uses
  vst2intf,vst2plugui;

type

  { TV02XTest3 }

  TV02XTest3 = class(TVGuiPlugin)
  public
    constructor Create(AHost:THostCallback);override;
  end;

implementation

uses
  v02xtest3editor;

{ TV02XTest3 }

constructor TV02XTest3.Create(AHost:THostCallback);
begin
  inherited Create(AHost);
  Base.SetUniqueID('PZn5');
  Base.SetNames('v02xtest3','PeaZomboss','vst24pas: v02xtest3');
  Base.SetVersion($00020005,$00020005);
  Editor.SetGui(TFormain);
  TFormain(Editor.Gui).Plugin:=self;
end;

end.

