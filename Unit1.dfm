object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 387
  ClientWidth = 463
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
  object Button1: TButton
    Left = 380
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 400
    Top = 40
  end
end
