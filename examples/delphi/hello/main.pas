unit main;

interface

uses
  vst2intf;

type
  TMyPlugin = class
  private
    FHost:THostCallback;
    FEffect:TAEffect;
  public
    constructor Create(AHost:THostCallback);
    destructor Destroy;override;
    function GetEffect:PAEffect;
    function dispatcher(opcode:TAEffectOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
    procedure process32(inputs,outputs:TBuffer32;nframes:Integer);
  end;

implementation

function DispatcherCallback(e:PAEffect;opcode,index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;cdecl;
var
  v:TMyPlugin;
begin
  v:=TMyPlugin(e^.Obj);
  if (opcode>=0) and (opcode<kVstAEOpcodeMax) then
    if opcode<>ord(effClose) then
      Result:=v.dispatcher(TAEffectOpcodes(opcode),index,value,ptr,opt)
    else begin
      v.dispatcher(TAEffectOpcodes(effClose),0,0,nil,0);
      v.Free;
      Result:=1;
    end
  else
    Result:=0;
end;

procedure Process32Callback(e:PAEffect;inputs,outputs:TBuffer32;nframes:Integer);cdecl;
begin
  TMyPlugin(e^.Obj).process32(inputs,outputs,nframes);
end;

constructor TMyPlugin.Create(AHost:THostCallback);
begin
  FHost:=AHost;
  FEffect.Magic:=kEffectMagic;
  FEffect.UniqueID:=MakeLong('PZd0');
  FEffect.Obj:=self;
  FEffect.Dispatcher:=@DispatcherCallback;
  FEffect.Process:=@Process32Callback;
  FEffect.NumInputs:=2;
  FEffect.NumOutputs:=2;
end;

destructor TMyPlugin.Destroy;
begin
  inherited Destroy;
end;

function TMyPlugin.GetEffect:PAEffect;
begin
  Result:=@FEffect;
end;

function TMyPlugin.dispatcher(opcode:TAEffectOpcodes;index:Int32;value:IntPtr;ptr:Pointer;opt:Single):IntPtr;
begin
  Result:=0;
  case opcode of
    effGetVstVersion:Result:=kVstVersion;
    else;
  end;
end;

{$PointerMath On}
procedure TMyPlugin.process32(inputs,outputs:TBuffer32;nframes:Integer);
var
  i:Integer;
begin
  for i:=0 to nframes-1 do
  begin
    outputs[0,i]:=inputs[0,i]*0.5;
    outputs[1,i]:=inputs[1,i]*0.5;
  end;
end;

end.


