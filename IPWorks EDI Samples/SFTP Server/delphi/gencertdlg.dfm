object Formgencertdl: TFormgencertdl
  Left = 0
  Top = 0
  Caption = 'New Certificate'
  ClientHeight = 180
  ClientWidth = 282
  Color = clBtnFace
  Constraints.MaxHeight = 218
  Constraints.MaxWidth = 298
  Constraints.MinHeight = 218
  Constraints.MinWidth = 298
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CertDetailGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 137
    Caption = 'Certificate Details'
    TabOrder = 0
    object lblNewCertDetails: TLabel
      Left = 56
      Top = 25
      Width = 40
      Height = 13
      Caption = 'Subject:'
    end
    object lblNewCertSerial: TLabel
      Left = 26
      Top = 52
      Width = 70
      Height = 13
      Caption = 'Serial Number:'
    end
    object lblNewCertExp: TLabel
      Left = 6
      Top = 79
      Width = 90
      Height = 13
      Caption = 'Expiration (years):'
    end
    object lblNewCertPass: TLabel
      Left = 46
      Top = 106
      Width = 50
      Height = 13
      Caption = 'Password:'
    end
    object txtNewCertSubject: TEdit
      Left = 102
      Top = 22
      Width = 153
      Height = 21
      TabOrder = 0
    end
    object txtNewCertPass: TEdit
      Left = 102
      Top = 103
      Width = 153
      Height = 21
      PasswordChar = '*'
      TabOrder = 1
    end
    object txtNewCertSerialUpDown: TUpDown
      Left = 167
      Top = 49
      Width = 16
      Height = 21
      Associate = txtNewCertSerial
      TabOrder = 2
    end
    object txtNewCertSerial: TEdit
      Left = 102
      Top = 49
      Width = 65
      Height = 21
      TabOrder = 3
      Text = '0'
    end
    object txtNewCertExp: TEdit
      Left = 102
      Top = 76
      Width = 65
      Height = 21
      TabOrder = 4
      Text = '0'
    end
    object txtNewCertExpUpDown: TUpDown
      Left = 167
      Top = 76
      Width = 16
      Height = 21
      Associate = txtNewCertExp
      TabOrder = 5
    end
  end
  object btnNewCertCancel: TButton
    Left = 198
    Top = 151
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnNewCertGenerate: TButton
    Left = 117
    Top = 151
    Width = 75
    Height = 25
    Caption = 'Generate'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnNewCertGenerateClick
  end
  object ibeCertMgr1: TibeCertMgr
    CertStore = 'MY'
    Left = 232
    Top = 64
  end
end
