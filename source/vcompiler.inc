{ Compiler Directives }

{$ifdef FPC}
  {$mode Delphi}
{$endif}

{ Define VST Version }
{$define VST_2_1_EXTENSIONS} // Version 2.1 extensions (08-06-2000)
{$define VST_2_2_EXTENSIONS} // Version 2.2 extensions (08-06-2001)
{$define VST_2_3_EXTENSIONS} // Version 2.3 extensions (20-05-2003)
{$define VST_2_4_EXTENSIONS} // Version 2.4 extensions (01-01-2006)

{ Disable for Hosts to serve Plug-ins below VST 2.4 }
{$ifdef VST_2_4_EXTENSIONS}
  {$define VST_FORCE_DEPRECATED}
{$endif}

{ Compiler switches }
{$H+}{$I-}{$J-}{$R-}{$Z1}

