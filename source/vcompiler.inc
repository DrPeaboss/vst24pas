{ Compiler Directives }

{$ifdef FPC}
  {$mode delphi}
  {$ifopt D+}
    {$define debug}
  {$endif}
{$endif}

// In fpc delphi mode, FPC default close
// In delphi, supported after delphi 2009
{$PointerMath on}

// Define VST Version extensions
{$define VST_2_1_EXTENSIONS} // 2.1 (2000/06/08)
{$define VST_2_2_EXTENSIONS} // 2.2 (2001/06/08)
{$define VST_2_3_EXTENSIONS} // 2.3 (2003/05/20)
{$define VST_2_4_EXTENSIONS} // 2.4 (2006/01/01)

// Disable for Hosts to serve Plug-ins below VST 2.4
{$ifdef VST_2_4_EXTENSIONS}
  {$define VST_FORCE_DEPRECATED}
{$endif}

// Compiler switches
{$H+}{$I-}{$J-}{$R-}

