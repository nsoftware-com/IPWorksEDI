object Formsftpserver: TFormsftpserver
  Left = 0
  Top = 0
  Caption = 'SFTP Server Demo'
  ClientHeight = 429
  ClientWidth = 635
  Color = clBtnFace
  Constraints.MaxHeight = 467
  Constraints.MaxWidth = 651
  Constraints.MinHeight = 467
  Constraints.MinWidth = 651
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 8
    Top = 8
    Width = 605
    Height = 26
    Caption = 
      'This demo shows how to use the SFTPServer component to create a ' +
      'simple SFTP server. Specify a server key (one is provided'#13#10'for y' +
      'ou) and then press Start to begin listening for incoming connect' +
      'ions.'
  end
  object btnStart: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartClick
  end
  object btnStop: TButton
    Left = 89
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 1
    OnClick = btnStopClick
  end
  object StatusOutput: TPageControl
    Left = 8
    Top = 71
    Width = 619
    Height = 350
    ActivePage = StatusDisplay
    TabOrder = 2
    object StatusDisplay: TTabSheet
      Caption = 'Status'
      object eventLogLabel: TLabel
        Left = 0
        Top = 0
        Width = 48
        Height = 13
        Caption = 'Event Log'
      end
      object txtEventLog: TMemo
        Left = 3
        Top = 19
        Width = 605
        Height = 300
        ScrollBars = ssVertical
        TabOrder = 0
        OnChange = txtEventLogChange
      end
    end
    object ConnectionDisplay: TTabSheet
      Caption = 'Connection'
      ImageIndex = 1
      object portLabel: TLabel
        Left = 16
        Top = 19
        Width = 24
        Height = 13
        Caption = 'Port:'
      end
      object rootDirLabel: TLabel
        Left = 16
        Top = 59
        Width = 74
        Height = 13
        Caption = 'Root Directory:'
      end
      object txtPort: TEdit
        Left = 96
        Top = 16
        Width = 41
        Height = 21
        TabOrder = 0
        Text = '22'
      end
      object txtRootDir: TEdit
        Left = 96
        Top = 56
        Width = 393
        Height = 21
        TabOrder = 1
        Text = '../../'
      end
      object rootDirSelectButton: TButton
        Left = 504
        Top = 54
        Width = 75
        Height = 25
        Caption = 'Browse'
        TabOrder = 2
        OnClick = rootDirSelectButtonClick
      end
    end
    object UserDisplay: TTabSheet
      Caption = 'Users'
      ImageIndex = 2
      object usrAccountsLabel: TLabel
        Left = 3
        Top = 3
        Width = 69
        Height = 13
        Caption = 'User Accounts'
      end
      object createUserNameLabel: TLabel
        Left = 77
        Top = 240
        Width = 26
        Height = 13
        Caption = 'User:'
      end
      object createPasswordLabel: TLabel
        Left = 53
        Top = 272
        Width = 50
        Height = 13
        Caption = 'Password:'
      end
      object txtUserList: TListView
        Left = 12
        Top = 22
        Width = 422
        Height = 187
        Columns = <
          item
            Caption = 'User'
            MinWidth = 100
            Width = 100
          end
          item
            Caption = 'Password'
            MinWidth = 150
            Width = 150
          end>
        Items.ItemData = {
          05760000000200000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
          00047400650073007400047400650073007400E894141600000000FFFFFFFFFF
          FFFFFF01000000FFFFFFFF00000000076D0079005F0075007300650072000B6D
          0079005F00700061007300730077006F007200640000651416FFFFFFFF}
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
      object deleteUserButton: TButton
        Left = 440
        Top = 184
        Width = 75
        Height = 25
        Caption = 'Delete User'
        TabOrder = 1
        OnClick = deleteUserButtonClick
      end
      object txtCreateUserName: TEdit
        Left = 120
        Top = 240
        Width = 121
        Height = 21
        TabOrder = 2
      end
      object txtCreatePassword: TEdit
        Left = 120
        Top = 267
        Width = 121
        Height = 21
        TabOrder = 3
      end
      object createUserButton: TButton
        Left = 272
        Top = 264
        Width = 75
        Height = 25
        Caption = 'Create User'
        TabOrder = 4
        OnClick = createUserButtonClick
      end
    end
    object KeyDisplay: TTabSheet
      Caption = 'Server Key'
      ImageIndex = 3
      object Label1: TLabel
        Left = 3
        Top = 64
        Width = 110
        Height = 13
        Caption = 'Certificate Store Type:'
      end
      object Label2: TLabel
        Left = 3
        Top = 93
        Width = 126
        Height = 13
        Caption = 'Certificate Store Location:'
      end
      object Label3: TLabel
        Left = 3
        Top = 119
        Width = 132
        Height = 13
        Caption = 'Certificate Store Password:'
      end
      object Label4: TLabel
        Left = 3
        Top = 145
        Width = 93
        Height = 13
        Caption = 'Certificate Subject:'
      end
      object txtCertStoreType: TComboBox
        Left = 143
        Top = 61
        Width = 201
        Height = 21
        ItemIndex = 2
        TabOrder = 0
        Text = 'PFX File'
        OnSelect = txtCertStoreTypeSelect
        Items.Strings = (
          'User'
          'Machine'
          'PFX File'
          'PEM Key File')
      end
      object txtCertStorePath: TEdit
        Left = 143
        Top = 88
        Width = 201
        Height = 21
        TabOrder = 1
        Text = '../../sftpserver.pfx'
      end
      object txtCertStorePassword: TEdit
        Left = 143
        Top = 115
        Width = 201
        Height = 21
        PasswordChar = '*'
        TabOrder = 2
        Text = 'demo'
      end
      object txtCertSubject: TEdit
        Left = 143
        Top = 142
        Width = 201
        Height = 21
        TabOrder = 3
        Text = '*'
      end
      object btnSelectCert: TButton
        Left = 3
        Top = 16
        Width = 126
        Height = 25
        Caption = 'Select Certificate'
        TabOrder = 4
        OnClick = btnSelectCertClick
      end
      object btnGenerateCert: TButton
        Left = 135
        Top = 16
        Width = 126
        Height = 25
        Caption = 'Generate Certificate'
        TabOrder = 5
        OnClick = btnGenerateCertClick
      end
    end
  end
  object ibeSFTPServer1: TibeSFTPServer
    DefaultAuthMethods = 'password,publickey'
    OnConnected = ibeSFTPServer1Connected
    OnConnectionRequest = ibeSFTPServer1ConnectionRequest
    OnDirCreate = ibeSFTPServer1DirCreate
    OnDirRemove = ibeSFTPServer1DirRemove
    OnDisconnected = ibeSFTPServer1Disconnected
    OnError = ibeSFTPServer1Error
    OnFileClose = ibeSFTPServer1FileClose
    OnFileOpen = ibeSFTPServer1FileOpen
    OnFileRemove = ibeSFTPServer1FileRemove
    OnSSHStatus = ibeSFTPServer1SSHStatus
    OnSSHUserAuthRequest = ibeSFTPServer1SSHUserAuthRequest
    Left = 600
    Top = 40
  end
end


