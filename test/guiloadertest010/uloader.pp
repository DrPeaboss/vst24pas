unit uloader;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Linux}dynlibs,{$endif}Classes, SysUtils, vst24pas.core;

type
  TVSTMain = function(vsthost:TVstHostCallback):PAEffect;cdecl;

  TPlugInfo = record
    FileName:string;
    PlugHandle:TLibHandle;
    PlugEffect:PAEffect;
    IsLoaded:Boolean;
  end;
  TPlugInfos = array[0..9] of TPlugInfo;

  { TPlugManager }

  TPlugManager = class(TComponent)
  private
    FPlugNumber:Integer;
    FPlugInfos:TPlugInfos;
    function GetAEffect(index:integer):PAEffect;
    function FindEffect(e:PAEffect):integer;
    function GetPlugFileName(index:integer):string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TryLoadPlugin(const libname:string;ID:integer):Boolean;
    function TryUnLoad(ID:Int32):Boolean;
    function Dispatcher(PlugID:int32;opcode:TAEffectOpcodes;index:int32=0;value:IntPtr=0;ptr:Pointer=nil;opt:single=0):IntPtr;
    function Host(ID:Int32;opcode:TAudioMasterOpcodes;index:int32=0;value:IntPtr=0;ptr:Pointer=nil;opt:single=0):IntPtr;
    procedure InitPlugin(ID:Int32);
    function IsPlugLoaded(ID:Int32):Boolean;
    function HasEditor(ID:Int32):Boolean;
    property PlugNumber:integer read FPlugNumber;
    property AEffect[index:integer]:PAEffect read GetAEffect;
    property PlugFileName[index:integer]:string read GetPlugFileName;
  end;

function HostCallback(effect: PAEffect; opcode: TAudioMasterOpcodes; index: Int32;
    Value: IntPtr; ptr: Pointer; opt: single): IntPtr; cdecl;

implementation

uses
  ufrmmain;

function HostCallback(effect: PAEffect; opcode: TAudioMasterOpcodes; index: Int32; Value: IntPtr; ptr: Pointer;
  opt: single): IntPtr; cdecl;
var
  pm:TPlugManager;
begin
  if IsConsole then
  begin
    if integer(opcode)<=ord(amGetInputSpeakerArrangement) then
      Writeln('opcode: ',opcode,' index: ',index,' value: ',value,
        ' ptr: ',IntToHex(ToIntPtr(ptr)),' opt: ', opt:5:5)
    else
      Writeln('Unknown opcode: ',Integer(opcode),' index: ',index,' value: ',value,
        ' ptr: ',IntToHex(ToIntPtr(ptr)),' opt: ', opt:5:5,' ',strpas(ptr));
  end;

  if Assigned(effect) and (effect^.Resvd1<>0) then
  begin
    pm:=TPlugManager(effect^.Resvd1);
    Result:=pm.Host(pm.FindEffect(effect),opcode,index,value,ptr,opt);
  end else
  begin
    Result:=0;
    case opcode of
      amVersion:Result:=kVstVersion;
      amGetSampleRate:Result:=44100;
      amGetBlockSize:Result:=1024;
      amGetProductString:VstStrncpy(ptr,'GuiLoaderTest010',31);
      amGetVendorString:VstStrncpy(ptr,'PeaZomboss',15);
      amGetVendorVersion:Result:=10;
      amCanDo:begin
                //if StrPas(ptr)='sizeWindow' then Result:=1;
                if IsConsole then Writeln('CanDo string: ',StrPas(ptr));
              end;
      else ;
    end;
  end;
end;

{ TPlugManager }

function TPlugManager.IsPlugLoaded(ID:Int32):Boolean;
begin
  Result:=False;
  if id in [0..9] then
    with FPlugInfos[id] do
      if IsLoaded then Result:=True;
end;

function TPlugManager.GetAEffect(index:integer):PAEffect;
begin
  Result:=nil;
  if IsPlugLoaded(index) then
    Result:=FPlugInfos[index].PlugEffect;
end;

function TPlugManager.FindEffect(e:PAEffect):integer;
var
  i:integer;
begin
  Result:=-1;
  for i:=0 to 9 do
    if FPlugInfos[i].PlugEffect=e then
    begin
      Result:=i;
      break;
    end;
end;

function TPlugManager.GetPlugFileName(index:integer):string;
begin
  if IsPlugLoaded(index) then
    Result:=FPlugInfos[index].FileName
  else
    Result:='';
end;

constructor TPlugManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if IsConsole then Writeln('Old 8087 CW: ',IntToHex(Get8087CW));
  if IsConsole then Writeln('Old MX CSR: ',IntToHex(GetMXCSR));
  //if IsConsole then Writeln('Old Exception Mask: ',IntToHex(DWord(GetExceptionMask)));
  // Attention here !
  // The following two lines are very very important !
  // Without them some plugins can not be loaded correctly !
  SetMXCSR($1FA0);
  Set8087CW($1337);
  // You can use below line to replace above 2 lines, with math unit.
  //SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
  if IsConsole then Writeln('New 8087 CW: ',IntToHex(Get8087CW));
  if IsConsole then Writeln('New MX CSR: ',IntToHex(GetMXCSR));
  //if IsConsole then Writeln('New Exception Mask: ',IntToHex(DWord(GetExceptionMask)));
end;

destructor TPlugManager.Destroy;
var
  i: Integer;
begin
  for i:=0 to 9 do
    if FPlugInfos[i].IsLoaded then
      TryUnLoad(i);
  inherited Destroy;
end;

function TPlugManager.TryLoadPlugin(const libname: string; ID: integer): Boolean;
var
  hLib:TLibHandle;
  vstmain:Pointer;
  eff:PAEffect;
begin
  if IsConsole then Writeln('Start load, lib name: ',libname,' ID: ',ID);
  Result:=False;
  hLib:=0;
  vstmain:=nil;
  eff:=nil;
  if not FileExists(libname) then Exit;
  if FPlugInfos[ID].IsLoaded then Exit;
  hLib:=LoadLibrary(libname);
  if hLib=0 then Exit;
  vstmain:=GetProcAddress(hLib,'VSTPluginMain');
  if vstmain=nil then
    vstmain:=GetProcAddress(hLib,'main');
  if vstmain=nil then Exit;
  try
    eff:=TVSTMain(vstmain)(@HostCallback);
  except
    on E:Exception do
    if IsConsole then Writeln(E.ClassName,': ',E.Message);
  end;
  if not Assigned(eff) then Exit;
  if eff^.Magic<>kEffectMagic then Exit;
  eff^.Resvd1 := ToIntPtr(self);
  Inc(FPlugNumber);
  with FPlugInfos[ID] do
  begin
    FileName:=libname;
    PlugHandle := hLib;
    PlugEffect := eff;
    IsLoaded := True;
  end;
  Result:=True;
  if IsConsole then Writeln('Loaded successfully');
end;

function TPlugManager.TryUnLoad(ID:Int32):Boolean;
begin
  if IsConsole then Writeln('Start unload, ID: ',ID);
  Result:=False;
  if not (ID in [0..9]) then Exit;
  with FPlugInfos[ID] do
    if IsLoaded then begin
      //Dispatcher(ID,effEditClose,0,0,nil,0);
      Dec(FPlugNumber);
      Dispatcher(ID,effClose,0,0,nil,0);
      FreeLibrary(PlugHandle);
      FileName:='';
      PlugEffect:=nil;
      PlugHandle:=0;
      IsLoaded := False;
      Result:=True;
      if IsConsole then Writeln('Unloaded successfully');
    end;
end;

function TPlugManager.Dispatcher(PlugID:int32; opcode:TAEffectOpcodes; index:int32; value:IntPtr; ptr:Pointer;
  opt:single):IntPtr;
begin
  with FPlugInfos[plugid] do
  if IsLoaded then
    Result:=PlugEffect^.Dispatcher(PlugEffect,opcode,index,value,ptr,opt);
end;

function TPlugManager.Host(ID:Int32; opcode:TAudioMasterOpcodes; index:int32; value:IntPtr; ptr:Pointer;
  opt:single):IntPtr;
begin
  Result:=0;
  if IsPlugLoaded(ID) then
  begin
    case opcode of
    amVersion:Result:=kVstVersion;
    amGetBlockSize:Result:=1024;
    amGetSampleRate:Result:=44100;
    amGetProductString:VstStrncpy(ptr,'GuiLoaderTest010',31);
    amGetVendorString:VstStrncpy(ptr,'PeaZomboss',15);
    amGetVendorVersion:Result:=10;
    amSizeWindow:begin
                   FormPlugManager.ResizeEditor(ID,index,value);
                   Result:=1;
                 end;
    amCanDo:begin
              if IsConsole then Writeln('CanDo string: ',StrPas(ptr));
              if StrPas(ptr)='sizeWindow' then Result:=1;
            end;
    amIdle:with FPlugInfos[ID] do PlugEffect^.Dispatcher(PlugEffect,effEditIdle,0,0,nil,0);
    else;
    end;
  end;
end;

procedure TPlugManager.InitPlugin(ID:Int32);
begin
  if IsConsole then Writeln('Init plugin, ID: ',ID);
  Dispatcher(ID,effOpen,0,0,nil,0);
  Dispatcher(ID,effSetSampleRate,0,0,nil,44100);
  Dispatcher(ID,effSetBlockSize,0,1024,nil,0);
  Dispatcher(ID,effMainsChanged,0,1,nil,0);
end;

function TPlugManager.HasEditor(ID:Int32):Boolean;
begin
  Result:=False;
  if IsPlugLoaded(ID) then
    with FPlugInfos[ID] do
      Result:=effFlagsHasEditor in PlugEffect^.Flags;
end;

end.

