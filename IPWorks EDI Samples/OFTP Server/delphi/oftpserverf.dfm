object FormOFTPServer: TFormOFTPServer
  Left = 206
  Top = 77
  BorderStyle = bsSingle
  Caption = 'OFTPServer Demo'
  ClientHeight = 456
  ClientWidth = 768
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
    Width = 399
    Height = 13
    Caption = 
      'The OFTPServer demo shows how to receive and send files from/to ' +
      'an OFTP client.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMenuHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 167
    Width = 752
    Height = 82
    Caption = 'Client Profile Settings'
    TabOrder = 0
    object Label3: TLabel
      Left = 16
      Top = 23
      Width = 44
      Height = 13
      Caption = 'Client Id:'
    end
    object Label4: TLabel
      Left = 16
      Top = 50
      Width = 80
      Height = 13
      Caption = 'Client Password:'
    end
    object Label5: TLabel
      Left = 260
      Top = 23
      Width = 94
      Height = 13
      Caption = 'Incoming Directory:'
    end
    object Label6: TLabel
      Left = 259
      Top = 50
      Width = 95
      Height = 13
      Caption = 'Outgoing Directory:'
    end
    object tClientId: TEdit
      Left = 106
      Top = 20
      Width = 148
      Height = 21
      TabOrder = 0
      Text = 'CLIENTSSID'
    end
    object tClientPassword: TEdit
      Left = 106
      Top = 47
      Width = 147
      Height = 21
      TabOrder = 1
      Text = 'PASSWORD'
    end
    object tIncomingDirectory: TEdit
      Left = 360
      Top = 20
      Width = 335
      Height = 21
      TabOrder = 2
    end
    object tOutgoingDirectory: TEdit
      Left = 360
      Top = 47
      Width = 335
      Height = 21
      TabOrder = 3
    end
    object bIncomingFolder: TButton
      Left = 701
      Top = 20
      Width = 36
      Height = 21
      Caption = '...'
      TabOrder = 4
      OnClick = bIncomingFolderClick
    end
    object bOutgoingFolder: TButton
      Left = 701
      Top = 47
      Width = 36
      Height = 21
      Caption = '...'
      TabOrder = 5
      OnClick = bOutgoingFolderClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 27
    Width = 752
    Height = 134
    Caption = 'Server Settings'
    TabOrder = 1
    object Label2: TLabel
      Left = 16
      Top = 23
      Width = 49
      Height = 13
      Caption = 'Server Id:'
    end
    object Label7: TLabel
      Left = 16
      Top = 77
      Width = 59
      Height = 13
      Caption = 'Server Port:'
    end
    object Label9: TLabel
      Left = 16
      Top = 50
      Width = 85
      Height = 13
      Caption = 'Server Password:'
    end
    object Label8: TLabel
      Left = 295
      Top = 23
      Width = 442
      Height = 43
      AutoSize = False
      Caption = 
        'If a certificate is specified here, it must have a private key. ' +
        'It will be used for SSL support, as well as Signing and Decrypti' +
        'on operations. Note that only clients supporting Version 2.0 wil' +
        'l be able to connect if SSL is enabled.'
      WordWrap = True
    end
    object tServerId: TEdit
      Left = 107
      Top = 20
      Width = 182
      Height = 21
      TabOrder = 0
      Text = 'SERVERSSID'
    end
    object tServerPort: TEdit
      Left = 107
      Top = 74
      Width = 182
      Height = 21
      TabOrder = 1
      Text = '3305'
    end
    object tServerPassword: TEdit
      Left = 107
      Top = 47
      Width = 182
      Height = 21
      TabOrder = 2
      Text = 'PASSWORD'
    end
    object bStart: TButton
      Left = 107
      Top = 101
      Width = 65
      Height = 25
      Caption = 'Start'
      TabOrder = 3
      OnClick = bStartClick
    end
    object bSelectCert: TButton
      Left = 295
      Top = 72
      Width = 115
      Height = 25
      Caption = 'Select Certificate'
      TabOrder = 4
      OnClick = bSelectCertClick
    end
    object chkUseSSL: TCheckBox
      Left = 295
      Top = 103
      Width = 66
      Height = 25
      Caption = 'Use SSL'
      TabOrder = 5
      OnClick = chkUseSSLClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 255
    Width = 752
    Height = 193
    Caption = 'Log'
    TabOrder = 2
    object tLog: TMemo
      Left = 10
      Top = 16
      Width = 727
      Height = 161
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Options = [ofEnableSizing]
    Left = 560
    Top = 8
  end
  object OFTPServer1: TibeOFTPServer
    CertStore = 'MY'
    SSLCertStore = 'MY'
    OnAcceptConnection = OFTPServer1AcceptConnection
    OnAcceptFile = OFTPServer1AcceptFile
    OnDisconnected = OFTPServer1Disconnected
    OnEndTransfer = OFTPServer1EndTransfer
    OnError = OFTPServer1Error
    OnReadyToSend = OFTPServer1ReadyToSend
    OnSSLStatus = OFTPServer1SSLStatus
    OnStartTransfer = OFTPServer1StartTransfer
    Left = 496
    Top = 8
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = OnTimer
    Left = 608
    Top = 7
  end
end


