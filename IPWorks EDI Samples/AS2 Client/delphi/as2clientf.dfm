object FormAs2client: TFormAs2client
  Left = 206
  Top = 77
  BorderStyle = bsSingle
  Caption = 'AS2Client Demo'
  ClientHeight = 756
  ClientWidth = 730
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 638
    Height = 39
    Caption = 
      'To send EDI data specify the URL and the data to be sent and cli' +
      'ck Send. To sign and/or encrypt, click the appropriate checkbox(' +
      'es) and you will be prompted to choose the appropriate certifica' +
      'tes. The localhost URL below will work if you first build the AS' +
      '2 Server demo included with the package.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 54
    Width = 714
    Height = 283
    Caption = ' Request Options '
    TabOrder = 0
    object Label2: TLabel
      Left = 15
      Top = 24
      Width = 51
      Height = 13
      Caption = 'AS2-From:'
    end
    object Label3: TLabel
      Left = 15
      Top = 51
      Width = 39
      Height = 13
      Caption = 'AS2-To:'
    end
    object Label4: TLabel
      Left = 15
      Top = 78
      Width = 23
      Height = 13
      Caption = 'URL:'
    end
    object Label5: TLabel
      Left = 15
      Top = 105
      Width = 71
      Height = 13
      Caption = 'Log Directory: '
    end
    object txtAs2From: TEdit
      Left = 88
      Top = 21
      Width = 575
      Height = 21
      TabOrder = 0
      Text = 'AS2 Test Sending Organization'
    end
    object txtAs2To: TEdit
      Left = 88
      Top = 48
      Width = 575
      Height = 21
      TabOrder = 1
      Text = 'AS2 Test Receiving Organization'
    end
    object txtURL: TEdit
      Left = 88
      Top = 75
      Width = 575
      Height = 21
      TabOrder = 2
      Text = 'http://localhost:8000'
    end
    object txtLogDir: TEdit
      Left = 88
      Top = 102
      Width = 575
      Height = 21
      TabOrder = 3
      Text = '..\..\logs'
    end
    object btnBrowseLogDir: TButton
      Left = 669
      Top = 97
      Width = 27
      Height = 25
      Caption = '...'
      TabOrder = 4
      OnClick = btnBrowseLogDirClick
    end
    object chkSigned: TCheckBox
      Left = 15
      Top = 136
      Width = 71
      Height = 17
      Caption = 'Signed'
      TabOrder = 5
      OnClick = chkSignedClick
    end
    object chkEncrypted: TCheckBox
      Left = 112
      Top = 136
      Width = 81
      Height = 17
      Caption = 'Encrypted'
      TabOrder = 6
      OnClick = chkEncryptedClick
    end
    object chkCompressed: TCheckBox
      Left = 232
      Top = 136
      Width = 105
      Height = 17
      Caption = 'Compressed'
      TabOrder = 7
    end
    object chkRequestSignature: TCheckBox
      Left = 360
      Top = 136
      Width = 113
      Height = 17
      Caption = 'Request Signature'
      TabOrder = 8
    end
    object btnSend: TButton
      Left = 615
      Top = 132
      Width = 81
      Height = 25
      Caption = 'Send'
      TabOrder = 9
      OnClick = btnSendClick
    end
    object txtEDIData: TMemo
      Left = 15
      Top = 163
      Width = 682
      Height = 110
      Lines.Strings = (
        'Paste EDI data here.')
      TabOrder = 10
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 343
    Width = 714
    Height = 405
    Caption = ' Response From Server '
    TabOrder = 1
    object Label6: TLabel
      Left = 15
      Top = 24
      Width = 171
      Height = 13
      Caption = 'The HTTP headers of the response:'
    end
    object Label7: TLabel
      Left = 15
      Top = 114
      Width = 284
      Height = 13
      Caption = 'The entire content of the receipt returned from the server:'
    end
    object Label8: TLabel
      Left = 15
      Top = 255
      Width = 259
      Height = 13
      Caption = 'The human-readable message returned in the receipt:'
    end
    object Label9: TLabel
      Left = 335
      Top = 255
      Width = 158
      Height = 13
      Caption = 'The MDN returned in the receipt:'
    end
    object txtHeaders: TMemo
      Left = 15
      Top = 43
      Width = 682
      Height = 65
      Lines.Strings = (
        'Reply Headers')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object txtReceipt: TMemo
      Left = 15
      Top = 133
      Width = 681
      Height = 116
      Lines.Strings = (
        'Receipt')
      ScrollBars = ssVertical
      TabOrder = 1
    end
    object txtReceiptMessage: TMemo
      Left = 15
      Top = 274
      Width = 314
      Height = 119
      Lines.Strings = (
        'Receipt Message')
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object txtMDN: TMemo
      Left = 335
      Top = 274
      Width = 361
      Height = 119
      Lines.Strings = (
        'MDN')
      ScrollBars = ssVertical
      TabOrder = 3
    end
  end
  object as2sender1: TibeAS2Sender
    EncryptionAlgorithm = '3DES'
    MDNOptions = 
      'signed-receipt-protocol=optional, pkcs7-signature; signed-receip' +
      't-micalg=optional, sha-256'
    ReceiptSignerCertStore = 'MY'
    RolloverSigningCertStore = 'MY'
    SignatureAlgorithm = 'sha-256'
    SigningCertStore = 'MY'
    SSLCertStore = 'MY'
    UserAgent = 'IPWorks EDI AS2Sender Component - www.nsoftware.com'
    Left = 680
    Top = 8
  end
end


