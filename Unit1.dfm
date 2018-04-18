object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Strange Clock'
  ClientHeight = 405
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 360
    Height = 360
  end
  object Label1: TLabel
    Left = 144
    Top = 376
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 8
    Top = 372
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    Visible = False
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 88
    Top = 368
  end
end
