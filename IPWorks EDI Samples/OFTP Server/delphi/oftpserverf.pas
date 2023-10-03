(*
 * IPWorks EDI 2022 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks EDI in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksedi
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit oftpserverf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ibecore, StrUtils, ibetypes,
  ibeoftpclient, ibeoftpserver, certmgrf, IOUtils, FileCtrl;

type
  TFormOFTPServer = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    OpenDialog1: TOpenDialog;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    tServerId: TEdit;
    tServerPort: TEdit;
    tServerPassword: TEdit;
    bStart: TButton;
    OFTPServer1: TibeOFTPServer;
    GroupBox3: TGroupBox;
    tLog: TMemo;
    Label3: TLabel;
    tClientId: TEdit;
    Label4: TLabel;
    tClientPassword: TEdit;
    tIncomingDirectory: TEdit;
    tOutgoingDirectory: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    bSelectCert: TButton;
    Label8: TLabel;
    chkUseSSL: TCheckBox;
    bIncomingFolder: TButton;
    bOutgoingFolder: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure bStartClick(Sender: TObject);
    procedure bSelectCertClick(Sender: TObject);
    procedure chkUseSSLClick(Sender: TObject);
    procedure OFTPServer1AcceptConnection(Sender: TObject;
      ConnectionId: Integer; const ClientSSIDCode, ClientPassword: string;
      ClientVersion: Integer; var ServerVersion: Integer; var Accept: Boolean;
      var ErrorCode: Integer; var ErrorDescription: string);
    procedure OFTPServer1AcceptFile(Sender: TObject; ConnectionId: Integer;
      const VirtualFileName, VirtualFileDate, Destination, Originator: string;
      var Accept: Boolean; var Filename: string; var Overwrite: Boolean;
      var ErrorCode: Integer; var ErrorDescription: string);
    procedure OFTPServer1Disconnected(Sender: TObject; ConnectionId,
      StatusCode: Integer; const Description: string);
    procedure OFTPServer1EndTransfer(Sender: TObject; ConnectionId,
      Direction: Integer; const LocalFile, VirtualFileName, VirtualFileDate,
      Destination, Originator: string; ReasonCode: Integer;
      const ReasonText: string; FileSize: Int64; const FileHash: string;
      var SendEndResponse: Boolean);
    procedure OFTPServer1ReadyToSend(Sender: TObject; ConnectionId: Integer);
    procedure OFTPServer1SSLStatus(Sender: TObject; ConnectionId: Integer;
      const Message: string);
    procedure OFTPServer1StartTransfer(Sender: TObject; ConnectionId,
      Direction: Integer; var LocalFile: string; const VirtualFileName,
      VirtualFileDate, Destination, Originator: string);
    procedure SendFilesToClient(ConnectionId: Integer);
    procedure OFTPServerOnSSLClientAuthentication(Sender: TObject;
      ConnectionId: Integer; CertEncoded: String; CertEncodedB: TBytes;
      const CertSubject: String; const CertIssuer: String; const Status: String;
      var Accept: Boolean);
    procedure OFTPServer1Error(Sender: TObject;
      ConnectionId: Integer; ErrorCode: Integer; const Description: String);
    procedure bIncomingFolderClick(Sender: TObject);
    procedure bOutgoingFolderClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);


  private
    { Private declarations }
    clientToSendFile: Integer;
  public
    { Public declarations }
  end;

var
  FormOFTPServer: TFormOFTPServer;

implementation

{$R *.dfm}

procedure TFormOFTPServer.bIncomingFolderClick(Sender: TObject);
var chosenDirectory : string;
begin
  if SelectDirectory('Select a directory', 'C:\', chosenDirectory) =  true then
  begin
    try
      tIncomingDirectory.Text := chosenDirectory;
    except on E:Exception do
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPServer.OnTimer(Sender: TObject);
begin
  Timer1.enabled := FALSE;
  try
    if clientToSendFile <> -1 then
    begin
      SendFilesToClient(clientToSendFile);
    end;
  finally
    clientToSendFile := -1;
    Timer1.enabled := True;
  end;
end;

procedure TFormOFTPServer.bOutgoingFolderClick(Sender: TObject);
var chosenDirectory : string;
begin
  if SelectDirectory('Select a directory', 'C:\', chosenDirectory) =  true then
  begin
    try
      tOutgoingDirectory.Text := chosenDirectory;
    except on E:Exception do
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPServer.bSelectCertClick(Sender: TObject);
begin
  FormCertmgr.lblCaption.Caption := 'Select a private key used to decrypt incoming data, and sign MDNs.';
  if (FormCertmgr.ShowModal = mrOK) then
  begin
    OFTPServer1.CertStoreType := TibeoftpserverCertStoreTypes(FormCertmgr.ibeCertMgr1.CertStoreType);
    OFTPServer1.CertStore := FormCertmgr.ibeCertMgr1.CertStore;
    OFTPServer1.CertStorePassword := FormCertmgr.ibeCertMgr1.CertStorePassword;
    OFTPServer1.CertSubject := FormCertmgr.ibeCertMgr1.CertSubject;
    chkUseSSL.Checked := True;
    chkUseSSLClick(Sender);
    //This certificate is also used for Secure Authentication, Signing, and Decryption operations.
    //This could be a separate certificate, but for simplicity this demo uses the same certificate
  end;

end;

procedure TFormOFTPServer.bStartClick(Sender: TObject);
begin

  try
    if bStart.Caption = 'Start' then
    begin
      // Start the server
      OFTPServer1.LocalPort := StrToInt(tServerPort.Text);
      OFTPServer1.ServerSSIDCode := tServerId.Text;
      OFTPServer1.ServerSFIDCode := OFTPServer1.ServerSSIDCode; // This is usually the same value as the SSID
      OFTPServer1.ServerPassword := tServerPassword.Text;
      OFTPServer1.Listening := true;
      tLog.Lines.Add('Server Has Started.');
      bStart.Caption := 'Stop';
    end else begin
      OFTPServer1.Shutdown();
      tLog.Lines.Add('Server Has Shutdown');
      bStart.Caption := 'Start';
    end;
  except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOK], 0);
  end;

end;

procedure TFormOFTPServer.chkUseSSLClick(Sender: TObject);
begin
  if (chkUseSSL.Checked) then
  begin
    OFTPServer1.UseSSL := True;
    OFTPServer1.SSLCertStoreType := OFTPServer1.CertStoreType;
    OFTPServer1.SSLCertStore := OFTPServer1.CertStore;
    OFTPServer1.SSLCertStorePassword := OFTPServer1.CertStorePassword;
    OFTPServer1.SSLCertSubject := OFTPServer1.CertSubject;
    tServerPort.Text := '6619';
  end else begin
    OFTPServer1.UseSSL := False;
    tServerPort.Text := '3305';
  end;
end;

procedure TFormOFTPServer.FormCreate(Sender: TObject);
begin
  clientToSendFile := -1;
end;

procedure TFormOFTPServer.OFTPServer1AcceptConnection(Sender: TObject;
  ConnectionId: Integer; const ClientSSIDCode, ClientPassword: string;
  ClientVersion: Integer; var ServerVersion: Integer; var Accept: Boolean;
  var ErrorCode: Integer; var ErrorDescription: string);
begin
  // This event fires when the client initially connects. Within this event, you
  // can validate the client's credentials by inspecting the ClientSSIDCode and
  // ClientPassword parameters and reject the connection if need be.
  tLog.Lines.Add('Client [' + ClientSSIDCode + '] connected.');

  // We must set the DownloadDirectory for incoming files here
  // as the client may start sending files immediately after this event completes.

  OFTPServer1.Connections[ConnectionId].DownloadDirectory := tIncomingDirectory.Text;

end;

procedure TFormOFTPServer.OFTPServer1AcceptFile(Sender: TObject;
  ConnectionId: Integer; const VirtualFileName, VirtualFileDate, Destination,
  Originator: string; var Accept: Boolean; var Filename: string;
  var Overwrite: Boolean; var ErrorCode: Integer; var ErrorDescription: string);
begin
  // This event fires whenever the client sends a file to the server.
  // By default it will be downloaded to the DownloadDirectory value specified
  // in the OFTPServer.Connections[x].DownloadDirectory field
  // Within this event you may choose to reject the file or override the location of the file */

  //If the SFID is empty do not accept the file
  if (Originator = '') then
    Accept := False
  else
    Accept := True;

  //Overwrite the file if it already exists in this demo.
  Overwrite := True;

end;

procedure TFormOFTPServer.OFTPServer1Disconnected(Sender: TObject; ConnectionId,
  StatusCode: Integer; const Description: string);
begin
  tLog.Lines.Add('Client [' + OFTPServer1.Connections[ConnectionId].SSIDCode + '] disconnected.');
end;

procedure TFormOFTPServer.OFTPServer1EndTransfer(Sender: TObject; ConnectionId,
  Direction: Integer; const LocalFile, VirtualFileName, VirtualFileDate,
  Destination, Originator: string; ReasonCode: Integer;
  const ReasonText: string; FileSize: Int64; const FileHash: string;
  var SendEndResponse: Boolean);
begin
  if (Direction = 0) then
  begin
    tLog.Lines.Add('Successfully received file "' + VirtualFileName + '" from client "' + OFTPServer1.Connections[ConnectionId].SSIDCode + '"');
  end else begin
    tLog.Lines.Add('Successfully sent file "' + VirtualFileName + '" to client "' + OFTPServer1.Connections[ConnectionId].SSIDCode + '"');
  end;
end;

procedure TFormOFTPServer.OFTPServer1Error(Sender: TObject;
  ConnectionId: Integer; ErrorCode: Integer; const Description: String);
begin
  tLog.Lines.Add(IntTOSTr(ErrorCode)+': '+Description);
end;

procedure TFormOFTPServer.OFTPServer1ReadyToSend(Sender: TObject;
  ConnectionId: Integer);
begin
  // When this event fires, it means that the connected client is now in a state
  // where it can receive files. We will set a flag to initiate send files to the
  // client at this time and call the SendFile method for each of the files to
  // be sent.
  clientToSendFile := ConnectionId;
end;

procedure TFormOFTPServer.OFTPServer1SSLStatus(Sender: TObject;
  ConnectionId: Integer; const Message: string);
begin
  tLog.Lines.Add('SSL Status [' + IntToStr(ConnectionId) + ']: ' + Message);
end;


procedure TFormOFTPServer.OFTPServer1StartTransfer(Sender: TObject;
  ConnectionId, Direction: Integer; var LocalFile: string;
  const VirtualFileName, VirtualFileDate, Destination, Originator: string);
begin
  if (Direction = 0) then
  begin
    tLog.Lines.Add('Receiving file "' + VirtualFileName + '" from client "' + OFTPServer1.Connections[ConnectionId].SSIDCode + '"');
  end else begin
    tLog.Lines.Add('Sending file "' + VirtualFileName + '" to client "' + OFTPServer1.Connections[ConnectionId].SSIDCode + '"');
  end;
end;

procedure TFormOFTPServer.SendFilesToClient(ConnectionId: Integer);
var Path: String;
begin
  tLog.Lines.Add(IntToStr(length(TDirectory.GetFiles(tOutgoingDirectory.Text))));
  for Path in TDirectory.GetFiles(tOutgoingDirectory.Text) do
  begin
    OFTPServer1.SendFile(ConnectionId, tClientId.Text, Path, '');
  end;
  oftpserver1.Logoff(ConnectionId);
end;


procedure TFormOFTPServer.OFTPServerOnSSLClientAuthentication(Sender: TObject;
  ConnectionId: Integer; CertEncoded: String; CertEncodedB: TBytes;
  const CertSubject: String; const CertIssuer: String; const Status: String;
  var Accept: Boolean);
begin
  // If SSLAuthenticateClients is set to true, a SSL client certificate will
  // be required when a client connects.
  // Use this event to accept or reject the certficate that is presented.

  if OFTPServer1.CertEncoded = CertEncoded then
    Accept := True;
end;

end.








