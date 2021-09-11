object FormGain: TFormGain
  Left = 0
  Top = 0
  Caption = 'FormGain'
  ClientHeight = 118
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelGain: TLabel
    Left = 17
    Top = 33
    Width = 46
    Height = 13
    Caption = 'LabelGain'
  end
  object CheckBoxHiFPS: TCheckBox
    Left = 201
    Top = 32
    Width = 65
    Height = 17
    Caption = 'HighFPS'
    TabOrder = 0
    OnClick = CheckBoxHiFPSClick
  end
  object ScrollBarGain: TScrollBar
    Left = 17
    Top = 81
    Width = 249
    Height = 22
    LargeChange = 5
    Max = 1000
    PageSize = 0
    PopupMenu = PopupMenuReset
    Position = 500
    TabOrder = 1
    OnChange = ScrollBarGainChange
  end
  object PopupMenuReset: TPopupMenu
    Left = 200
    Top = 72
    object MenuItemReset: TMenuItem
      Caption = 'Reset'
      OnClick = MenuItemResetClick
    end
  end
end
