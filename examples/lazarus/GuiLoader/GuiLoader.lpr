program GuiLoader;

{$mode objfpc}{$H+}
{$ifdef MSWINDOWS}{.$apptype console}{$endif} // For debug

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrmmain
  { you can add units after this };

{$R *.res}

begin
  if IsConsole then Writeln('Starting GuiLoaderTest010 ...');
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormPlugManager, FormPlugManager);
  Application.Run;
end.

