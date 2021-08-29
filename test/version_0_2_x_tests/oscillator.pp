unit oscillator;

{$mode ObjFPC}{$H+}

interface

const
  TwoPi=2*PI;

type
  TOscModes = (
    omSine,
    omSaw,
    omSquare,
    omTriangle
  );

  { TOscillator }
  // T should be Single or Double
  generic TOscillator<T> = class
  private type
    PT=^T;
  private
    FOscMode:TOscModes;
    FFreq:T;
    FPhase:T;
    FSampleRate:T;
    FPhaseIncrement:T;
    FGain:T;
    FIsMuted:Boolean;
    procedure UpdateIncrement;
    procedure SetFreq(AValue:T);
    procedure SetSampleRate(AValue:T);
  public
    constructor Create;
    constructor Create(AOscMode:TOscModes;AFreq:T=440;APhase:T=0;ASampleRate:T=44100);
    procedure Generage(const Buf:PT;SampleFrames:Integer);
    function NextSample:T;
    property OscMode:TOscModes read FOscMode write FOscMode;
    property Freq:T read FFreq write SetFreq;
    property SampleRate:T read FSampleRate write SetSampleRate;
    property Gain:T read FGain write FGain;
    property IsMuted:Boolean read FIsMuted write FIsMuted;
  end;

  TSingleOsc=specialize TOscillator<Single>;
  TDoubleOsc=specialize TOscillator<Double>;


function NoteToFrequency(Note:Int8):Single;

implementation

function NoteToFrequency(Note:Int8):Single;
const
  StandardA4 = 440.0;
  NoteA4 = 69;
begin
  // Power(2,(Note-NoteA4)/12) = Exp((Note-NoteA4)/12*Ln(2))
  Result:=StandardA4*Exp((Note-NoteA4)*0.0577622650466621);
end;

{ TOscillator }

procedure TOscillator.UpdateIncrement;
begin
  FPhaseIncrement:=FFreq*2*PI/FSampleRate;
end;

procedure TOscillator.SetFreq(AValue:T);
begin
  FFreq:=AValue;
  UpdateIncrement;
end;

procedure TOscillator.SetSampleRate(AValue:T);
begin
  FSampleRate:=AValue;
  UpdateIncrement;
end;

constructor TOscillator.Create;
begin
  FFreq:=440;
  FPhase:=0;
  FSampleRate:=44100;
  UpdateIncrement;
  FGain:=1;
  FIsMuted:=True;
end;

constructor TOscillator.Create(AOscMode:TOscModes;AFreq:T;APhase:T;ASampleRate:T);
begin
  FOscMode:=AOscMode;
  FFreq:=AFreq;
  FPhase:=APhase;
  FSampleRate:=ASampleRate;
  UpdateIncrement;
  FGain:=1;
  FIsMuted:=True;
end;

procedure TOscillator.Generage(const Buf:PT;SampleFrames:Integer);
var
  i:Integer;
begin
  if FIsMuted then
    for i:=0 to SampleFrames-1 do
      Buf[i]:=0
  else
  case FOscMode of
    omSine: begin
      for i:=0 to SampleFrames-1 do
      begin
        Buf[i]:=sin(FPhase);
        Buf[i]:=Buf[i]*FGain;
        FPhase:=FPhase+FPhaseIncrement;
        while FPhase>=TwoPi do
          FPhase:=FPhase-TwoPi;
      end;
    end;
    omSaw: begin
      for i:=0 to SampleFrames-1 do
      begin
        Buf[i]:=1-(2*FPhase/TwoPi);
        Buf[i]:=Buf[i]*FGain;
        FPhase:=FPhase+FPhaseIncrement;
        while FPhase>=TwoPi do
          FPhase:=FPhase-TwoPi;
      end;
    end;
    omSquare: begin
      for i:=0 to SampleFrames-1 do
      begin
        if FPhase<=Pi then
          Buf[i]:=1.0
        else
          Buf[i]:=-1.0;
        Buf[i]:=Buf[i]*FGain;
        FPhase:=FPhase+FPhaseIncrement;
        while FPhase>=TwoPi do
          FPhase:=FPhase-TwoPi;
      end;
    end;
    omTriangle: begin
      for i:=0 to SampleFrames-1 do
      begin
        Buf[i]:=2.0 * ( abs(2.0*FPhase/TwoPi-1.0) - 0.5);
        Buf[i]:=Buf[i]*FGain;
        FPhase:=FPhase+FPhaseIncrement;
        while FPhase>=TwoPi do
          FPhase:=FPhase-TwoPi;
      end;
    end;
  end;
end;

function TOscillator.NextSample:T;
begin
  Result:=0;
  if IsMuted then Exit;
  case FOscMode of
    omSine: begin
      Result:=sin(FPhase);
    end;
    omSaw: begin
      Result:= 1-(2*FPhase/TwoPi)
    end;
    omSquare: begin
      if FPhase<PI then
        Result:=1
      else
        Result:=-1;
    end;
    omTriangle: begin
      Result:=2.0 * ( abs(2.0*FPhase/TwoPi-1.0) - 0.5);
    end;
  end;
  Result:=Result*FGain;
  FPhase:=FPhase+FPhaseIncrement;
  while FPhase>=TwoPi do
    FPhase:=FPhase-TwoPi;
end;

end.

