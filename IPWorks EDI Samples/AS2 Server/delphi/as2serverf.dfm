object FormAs2server: TFormAs2server
  Left = 245
  Top = 231
  Caption = 'AS2Receiver Demo'
  ClientHeight = 566
  ClientWidth = 773
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 737
    Height = 26
    Caption = 
      'This demo shows how to process incoming AS2 messages. Demo certi' +
      'ficates are already configured. You may configure different cert' +
      'ificates, or simply press Start Server to begin listening. Note:' +
      ' The component itself does not include a web server.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 271
    Width = 757
    Height = 287
    Caption = 'Server Info'
    TabOrder = 1
    DesignSize = (
      757
      287)
    object Label2: TLabel
      Left = 616
      Top = 23
      Width = 82
      Height = 13
      Caption = 'Web Server Port:'
    end
    object Label8: TLabel
      Left = 24
      Top = -104
      Width = 32
      Height = 13
      Caption = 'Label8'
    end
    object Label10: TLabel
      Left = 336
      Top = -96
      Width = 38
      Height = 13
      Caption = 'Label10'
    end
    object txtPort: TEdit
      Left = 704
      Top = 20
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '8000'
    end
    object mmoMessage: TMemo
      Left = 3
      Top = 47
      Width = 742
      Height = 228
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'Log Messages'
        '')
      ScrollBars = ssBoth
      TabOrder = 2
    end
    object btnStartServer: TButton
      Left = 3
      Top = 16
      Width = 75
      Height = 25
      Caption = '&Start Server'
      TabOrder = 0
      OnClick = btnStartServerClick
    end
    object GroupBox2: TGroupBox
      Left = 0
      Top = -144
      Width = 185
      Height = 105
      Caption = 'GroupBox2'
      TabOrder = 3
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = -136
      Width = 185
      Height = 105
      Caption = 'GroupBox3'
      TabOrder = 4
    end
    object GroupBox4: TGroupBox
      Left = 40
      Top = -144
      Width = 185
      Height = 105
      Caption = 'GroupBox4'
      TabOrder = 5
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 56
    Width = 757
    Height = 209
    Caption = 'AS2 Configuration'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 38
      Width = 104
      Height = 13
      Caption = 'Decryption Certificate:'
    end
    object Label4: TLabel
      Left = 16
      Top = 99
      Width = 83
      Height = 13
      Caption = 'Signer Certificate:'
    end
    object Label5: TLabel
      Left = 16
      Top = 19
      Width = 525
      Height = 13
      Caption = 
        'The decryption certificate is your private key. It is used to de' +
        'crypt incoming data. It is also used to sign the MDN'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label6: TLabel
      Left = 16
      Top = 80
      Width = 529
      Height = 13
      Caption = 
        'The recipient certificate is your partner'#39's public certificate. ' +
        'It is used to verify the signature of incoming messages.'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label7: TLabel
      Left = 16
      Top = 171
      Width = 66
      Height = 13
      Caption = 'Log Directory:'
    end
    object Label9: TLabel
      Left = 16
      Top = 136
      Width = 86
      Height = 13
      Caption = 'Receiving AS2 Id:'
    end
    object Label11: TLabel
      Left = 416
      Top = 136
      Width = 77
      Height = 13
      Caption = 'Sending AS2 Id:'
    end
    object txtDecryptionCert: TEdit
      Left = 126
      Top = 38
      Width = 587
      Height = 21
      TabOrder = 0
    end
    object btnBrowseDecryptionCert: TButton
      Left = 719
      Top = 33
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseDecryptionCertClick
    end
    object txtSignerCert: TEdit
      Left = 126
      Top = 99
      Width = 587
      Height = 21
      TabOrder = 2
    end
    object btnBrowseSignerCert: TButton
      Left = 719
      Top = 94
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 3
      OnClick = btnBrowseSignerCertClick
    end
    object txtLogDir: TEdit
      Left = 126
      Top = 171
      Width = 587
      Height = 21
      TabOrder = 6
      Text = '..\..\logs'
    end
    object btnBrowseLogDir: TButton
      Left = 719
      Top = 167
      Width = 27
      Height = 25
      Caption = '...'
      TabOrder = 7
      OnClick = btnBrowseLogDirClick
    end
    object txtAS2To: TEdit
      Left = 126
      Top = 133
      Width = 174
      Height = 21
      TabOrder = 4
      Text = 'AS2 Test Receiving Organization'
    end
    object Edit2: TEdit
      Left = 512
      Top = 133
      Width = 201
      Height = 21
      TabOrder = 5
      Text = 'AS2 Test Sending Organization'
    end
  end
  object AS2Receiver1: TibeAS2Receiver
    CertStore = 'MY'
    RolloverCertStore = 'MY'
    SSLAcceptServerCertStore = 'MY'
    SSLCertStore = 'MY'
    Left = 720
    Top = 24
  end
  object IdHTTPServer1: TIdHTTPServer
    Bindings = <>
    OnCommandGet = IdHTTPServer1CommandGet
    Left = 664
    Top = 24
  end
end


