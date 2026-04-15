object Form1: TForm1
  Left = 206
  Top = 225
  Width = 460
  Height = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 181
    Width = 452
    Height = 6
    Cursor = crVSplit
    Align = alTop
    Beveled = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 29
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object BitBtn1: TBitBtn
      Left = 0
      Top = 2
      Width = 249
      Height = 25
      Caption = 'Parse'
      Default = True
      TabOrder = 0
      OnClick = BitBtn1Click
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 29
    Width = 452
    Height = 152
    Align = alTop
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 0
    Top = 187
    Width = 452
    Height = 148
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object JvDragDrop1: TJvDragDrop
    OnDrop = JvDragDrop1Drop
    Left = 248
    Top = 112
  end
end
