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
unit oftpclientf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ibecore, StrUtils, ibetypes,
  ibeoftpclient, FileCtrl;

type
  TFormOFTPClient = class(TForm)
    Label1: TLabel;
    OFTPClient1: TibeOFTPClient;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    tTransfer: TMemo;
    bSend: TButton;
    GroupBox3: TGroupBox;
    bRetrieve: TButton;
    tOFTPServer: TEdit;
    tClientSSIDCode: TEdit;
    tClientSFIDCode: TEdit;
    tClientPassword: TEdit;
    tServerPort: TEdit;
    tServerSSIDCode: TEdit;
    tServerSFIDCode: TEdit;
    tServerPassword: TEdit;
    chkOverwrite: TCheckBox;
    OpenDialog1: TOpenDialog;
    bBrowseFileSend: TButton;
    Label10: TLabel;
    tFileToSend: TEdit;
    Label11: TLabel;
    tReceiveToFolder: TEdit;
    chkUseSSL: TCheckBox;
    bBrowseFolderRetrieve: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bRetrieveClick(Sender: TObject);
    procedure bSendClick(Sender: TObject);
    procedure bBrowseFileSendClick(Sender: TObject);
    procedure bBrowseFolderRetrieveClick(Sender: TObject);
    procedure OFTPClient1StartTransfer(Sender: TObject; Direction: Integer;
      var LocalFile: string; const VirtualFileName, VirtualFileDate,
      Destination, Originator: string);
    procedure OFTPClient1SSLServerAuthentication(Sender: TObject;
      CertEncoded: string; CertEncodedB: TBytes; const CertSubject,
      CertIssuer, Status: string; var Accept: Boolean);
    procedure chkUseSSLClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOFTPClient: TFormOFTPClient;

implementation

{$R *.dfm}

procedure TFormOFTPClient.bBrowseFileSendClick(Sender: TObject);
begin
  if OpenDialog1.Execute =  true then
  begin
    try
      tFileToSend.Text := OpenDialog1.FileName;
    except on E:Exception do
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPClient.bBrowseFolderRetrieveClick(Sender: TObject);
var chosenDirectory : string;
begin
  if SelectDirectory('Select a directory', 'C:\', chosenDirectory) =  true then
  begin
    try
      tReceiveToFolder.Text := chosenDirectory;
    except on E:Exception do
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPClient.bSendClick(Sender: TObject);
begin
  if tFileToSend.Text = '' then
  begin
    MessageDlg('No file specified.', mtInformation, [mbOk], 0);
    Exit;
  end;
  try
    if OFTPClient1.Connected = false then
    begin
      OFTPClient1.RemoteHost := tOFTPServer.Text;
      OFTPClient1.RemotePort := StrToInt(tServerPort.Text);
      OFTPClient1.ClientPassword := tClientPassword.Text;
      OFTPClient1.ClientSFIDCode := tClientSFIDCode.Text;
      OFTPClient1.ClientSSIDCode := tClientSSIDCode.Text;
      OFTPClient1.ServerPassword := tServerPassword.Text;
      OFTPClient1.ServerSFIDCode := tServerSFIDCode.Text;
      OFTPClient1.ServerSSIDCode := tServerSSIDCode.Text;
      OFTPClient1.UseSSL := chkUseSSL.Checked;
    end;
    OFTPClient1.SendFile(tFileToSend.Text, '');
    tTransfer.Lines.Add('Done.');
  except on E:Exception do
    begin
      tTransfer.Lines.Add('Failed.');
      OFTPClient1.Connected := false;
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPClient.chkUseSSLClick(Sender: TObject);
begin
  if (chkUseSSL.Checked) then
  begin
    tServerPort.Text := '6619';
  end else begin
    tServerPort.Text := '3305';
  end;
end;

procedure TFormOFTPClient.bRetrieveClick(Sender: TObject);
begin
  if tReceiveToFolder.Text = '' then
  begin
    MessageDlg('No local folder specified.', mtInformation, [mbOk], 0);
    Exit;
  end;
  try
    if OFTPClient1.Connected = false then
    begin
      OFTPClient1.RemoteHost := tOFTPServer.Text;
      OFTPClient1.RemotePort := StrToInt(tServerPort.Text);
      OFTPClient1.ClientPassword := tClientPassword.Text;
      OFTPClient1.ClientSFIDCode := tClientSFIDCode.Text;
      OFTPClient1.ClientSSIDCode := tClientSSIDCode.Text;
      OFTPClient1.ServerPassword := tServerPassword.Text;
      OFTPClient1.ServerSFIDCode := tServerSFIDCode.Text;
      OFTPClient1.ServerSSIDCode := tServerSSIDCode.Text;
      OFTPClient1.UseSSL := chkUseSSL.Checked;
    end;
    OFTPClient1.Overwrite := chkOverwrite.Checked;
    OFTPClient1.DownloadDirectory := tReceiveToFolder.Text;

    OFTPClient1.ReceiveFiles();
    tTransfer.Lines.Add('Done.');
  except on E:Exception do
    begin
      tTransfer.Lines.Add('Failed.');
      OFTPClient1.Connected := false;
      MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
    end;
  end;
end;

procedure TFormOFTPClient.FormCreate(Sender: TObject);
begin
  //TODO
end;

procedure TFormOFTPClient.OFTPClient1SSLServerAuthentication(Sender: TObject;
  CertEncoded: string; CertEncodedB: TBytes; const CertSubject,
  CertIssuer, Status: string; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TFormOFTPClient.OFTPClient1StartTransfer(Sender: TObject;
  Direction: Integer; var LocalFile: string; const VirtualFileName,
  VirtualFileDate, Destination, Originator: string);
begin
  if Direction = 0 then
  begin
    tTransfer.Lines.Add('Sending ' + VirtualFileName);
  end else begin
    tTransfer.Lines.Add('Receiving ' + VirtualFileName);
  end;
end;

end.




