object FormCertmgr: TFormCertmgr
  Left = 268
  Top = 159
  Caption = 'Certificate Selector'
  ClientHeight = 588
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblCaption: TLabel
    Left = 8
    Top = 8
    Width = 46
    Height = 13
    Caption = 'lblCaption'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 35
    Width = 425
    Height = 134
    Caption = 'Certificate Info'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 105
      Width = 76
      Height = 13
      Caption = 'Certificate store:'
    end
    object Label4: TLabel
      Left = 16
      Top = 24
      Width = 55
      Height = 13
      Caption = 'Store Type:'
    end
    object Label5: TLabel
      Left = 16
      Top = 51
      Width = 69
      Height = 13
      Caption = 'Certificate File:'
    end
    object Label6: TLabel
      Left = 16
      Top = 80
      Width = 99
      Height = 13
      Caption = 'Certificate Password:'
    end
    object CertificateStore: TComboBox
      Left = 121
      Top = 102
      Width = 177
      Height = 21
      TabOrder = 4
      Text = 'CertificateStore'
    end
    object cboStoreTypes: TComboBox
      Left = 121
      Top = 21
      Width = 177
      Height = 21
      TabOrder = 0
      Text = 'PFX File (.pfx,.p12)'
      OnChange = cboStoreTypesChange
      Items.Strings = (
        'PFX File (.pfx,.p12)'
        'Public Certificate File (.cer)'
        'Windows Store')
    end
    object btnBrowseCert: TButton
      Left = 383
      Top = 46
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 2
      OnClick = btnBrowseCertClick
    end
    object txtCertFile: TEdit
      Left = 121
      Top = 48
      Width = 256
      Height = 21
      TabOrder = 1
      Text = '..\..\as2receiver.pfx'
    end
    object txtCertStorePassword: TEdit
      Left = 121
      Top = 75
      Width = 256
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
      Text = 'test'
    end
  end
  object cmdOK: TButton
    Left = 270
    Top = 551
    Width = 82
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = cmdOKClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 175
    Width = 425
    Height = 370
    Caption = 'Certificate Selection'
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 27
      Width = 174
      Height = 13
      Caption = 'Available certificates (click to select):'
    end
    object Label3: TLabel
      Left = 16
      Top = 175
      Width = 130
      Height = 13
      Caption = 'Selected Certificate Details:'
    end
    object AvailableCertificates: TListBox
      Left = 16
      Top = 53
      Width = 393
      Height = 105
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnClick = AvailableCertificatesClick
    end
    object btnListCertificates: TButton
      Left = 320
      Top = 22
      Width = 89
      Height = 25
      Caption = '&List Certificates'
      TabOrder = 0
      OnClick = btnListCertificatesClick
    end
    object CertificateInfo: TMemo
      Left = 16
      Top = 194
      Width = 393
      Height = 161
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
    end
  end
  object btnCancel: TButton
    Left = 358
    Top = 551
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object ibeCertMgr1: TibeCertMgr
    CertStore = 'MY'
    OnCertList = ibeCertmgr1CertList
    OnStoreList = ibeCertmgr1StoreList
    Left = 376
    Top = 8
  end
end
