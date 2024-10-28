object FormOFTPClient: TFormOFTPClient
  Left = 206
  Top = 77
  BorderStyle = bsSingle
  Caption = 'OFTPClient Demo'
  ClientHeight = 547
  ClientWidth = 613
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
    Width = 578
    Height = 26
    Caption = 
      'All server information and credentials must be set to perform a ' +
      'send or receive. To send a file, first select the file on the lo' +
      'cal machine. To receive files, first select the directory on the' +
      ' local machine to which you would like the files written.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 51
    Width = 65
    Height = 13
    Caption = 'OFTP Server:'
  end
  object Label3: TLabel
    Left = 8
    Top = 78
    Width = 85
    Height = 13
    Caption = 'Client SSID Code:'
  end
  object Label4: TLabel
    Left = 8
    Top = 105
    Width = 85
    Height = 13
    Caption = 'Client SFID Code:'
  end
  object Label5: TLabel
    Left = 8
    Top = 132
    Width = 80
    Height = 13
    Caption = 'Client Password:'
  end
  object Label6: TLabel
    Left = 306
    Top = 105
    Width = 90
    Height = 13
    Caption = 'Server SFID Code:'
  end
  object Label7: TLabel
    Left = 306
    Top = 51
    Width = 59
    Height = 13
    Caption = 'Server Port:'
  end
  object Label8: TLabel
    Left = 306
    Top = 78
    Width = 90
    Height = 13
    Caption = 'Server SSID Code:'
  end
  object Label9: TLabel
    Left = 306
    Top = 132
    Width = 85
    Height = 13
    Caption = 'Server Password:'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 179
    Width = 597
    Height = 86
    Caption = 'Send'
    TabOrder = 0
    object Label10: TLabel
      Left = 10
      Top = 24
      Width = 60
      Height = 13
      Caption = 'File to Send:'
    end
    object bSend: TButton
      Left = 498
      Top = 48
      Width = 90
      Height = 25
      Caption = 'Send'
      TabOrder = 0
      OnClick = bSendClick
    end
    object bBrowseFileSend: TButton
      Left = 552
      Top = 21
      Width = 36
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = bBrowseFileSendClick
    end
    object tFileToSend: TEdit
      Left = 76
      Top = 21
      Width = 470
      Height = 21
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 367
    Width = 597
    Height = 172
    Caption = 'Transfer Info'
    TabOrder = 1
    object tTransfer: TMemo
      Left = 10
      Top = 16
      Width = 578
      Height = 145
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 271
    Width = 597
    Height = 90
    Caption = 'Retrieve'
    TabOrder = 2
    object Label11: TLabel
      Left = 10
      Top = 24
      Width = 61
      Height = 13
      Caption = 'Local Folder:'
    end
    object bRetrieve: TButton
      Left = 498
      Top = 48
      Width = 90
      Height = 25
      Caption = 'Retrieve'
      TabOrder = 0
      OnClick = bRetrieveClick
    end
    object tReceiveToFolder: TEdit
      Left = 77
      Top = 21
      Width = 469
      Height = 21
      TabOrder = 1
    end
    object bBrowseFolderRetrieve: TButton
      Left = 552
      Top = 21
      Width = 36
      Height = 21
      Caption = '...'
      TabOrder = 2
      OnClick = bBrowseFolderRetrieveClick
    end
  end
  object tOFTPServer: TEdit
    Left = 112
    Top = 48
    Width = 177
    Height = 21
    TabOrder = 3
    Text = 'localhost'
  end
  object tClientSSIDCode: TEdit
    Left = 112
    Top = 75
    Width = 177
    Height = 21
    TabOrder = 4
    Text = 'CLIENTSSID'
  end
  object tClientSFIDCode: TEdit
    Left = 112
    Top = 102
    Width = 177
    Height = 21
    TabOrder = 5
    Text = 'CLIENTSSID'
  end
  object tClientPassword: TEdit
    Left = 112
    Top = 129
    Width = 177
    Height = 21
    TabOrder = 6
    Text = 'PASSWORD'
  end
  object tServerPort: TEdit
    Left = 402
    Top = 48
    Width = 194
    Height = 21
    TabOrder = 7
    Text = '3305'
  end
  object tServerSSIDCode: TEdit
    Left = 402
    Top = 75
    Width = 194
    Height = 21
    TabOrder = 8
    Text = 'SERVERSSID'
  end
  object tServerSFIDCode: TEdit
    Left = 402
    Top = 102
    Width = 194
    Height = 21
    TabOrder = 9
    Text = 'SERVERSSID'
  end
  object tServerPassword: TEdit
    Left = 402
    Top = 129
    Width = 194
    Height = 21
    TabOrder = 10
    Text = 'PASSWORD'
  end
  object chkOverwrite: TCheckBox
    Left = 511
    Top = 156
    Width = 85
    Height = 25
    Caption = 'Overwrite'
    Checked = True
    State = cbChecked
    TabOrder = 11
  end
  object chkUseSSL: TCheckBox
    Left = 420
    Top = 156
    Width = 85
    Height = 25
    Caption = 'Use SSL'
    TabOrder = 12
    OnClick = chkUseSSLClick
  end
  object OFTPClient1: TibeOFTPClient
    CertStore = 'MY'
    DownloadDirectory = './'
    SSLCertStore = 'MY'
    OnSSLServerAuthentication = OFTPClient1SSLServerAuthentication
    OnStartTransfer = OFTPClient1StartTransfer
    Left = 576
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Options = [ofEnableSizing]
    Left = 568
    Top = 256
  end
end


