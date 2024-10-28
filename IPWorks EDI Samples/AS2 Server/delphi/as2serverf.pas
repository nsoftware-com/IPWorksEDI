(*
 * IPWorks EDI 2024 Delphi Edition - Sample Project
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
unit as2serverf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ibecore, ibetypes, ibecertmgr, certmgrf, ibeas2receiver, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdCustomHTTPServer, IdHTTPServer, IdContext;

type
  TFormAs2server = class(TForm)
    Label1: TLabel;
    AS2Receiver1: TibeAS2Receiver;
    IdHTTPServer1: TIdHTTPServer;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    txtPort: TEdit;
    mmoMessage: TMemo;
    btnStartServer: TButton;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label3: TLabel;
    txtDecryptionCert: TEdit;
    btnBrowseDecryptionCert: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtSignerCert: TEdit;
    btnBrowseSignerCert: TButton;
    Label7: TLabel;
    txtLogDir: TEdit;
    btnBrowseLogDir: TButton;
    Label8: TLabel;
    Label9: TLabel;
    txtAS2To: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Edit2: TEdit;
    procedure btnStartServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseDecryptionCertClick(Sender: TObject);
    procedure btnBrowseSignerCertClick(Sender: TObject);
    procedure IdHTTPServer1CommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure btnBrowseLogDirClick(Sender: TObject);
  private
    { Private declarations }
    procedure Log(msg: String);

  public
    { Public declarations }
  end;

var
  FormAs2server: TFormAs2server;

implementation

{$R *.DFM}

procedure TFormAs2server.btnBrowseDecryptionCertClick(Sender: TObject);
begin
FormCertmgr.lblCaption.Caption := 'Select a private key used to decrypt incoming data, and sign MDNs.';
if (FormCertmgr.ShowModal = mrOK) then
begin
  as2receiver1.CertStoreType := Tibeas2receiverCertStoreTypes(FormCertmgr.ibeCertMgr1.CertStoreType);
  as2receiver1.CertStore := FormCertmgr.ibeCertMgr1.CertStore;
  as2receiver1.CertStorePassword := FormCertmgr.ibeCertMgr1.CertStorePassword;
  as2receiver1.CertSubject := FormCertmgr.ibeCertMgr1.CertSubject;
  txtDecryptionCert.Text := as2receiver1.CertSubject;
end;
end;

procedure TFormAs2server.btnBrowseLogDirClick(Sender: TObject);
begin
with TFileOpenDialog.Create(nil) do
begin
  Options := [fdoPickFolders];
  if Execute then begin
    txtLogDir.Text := FileName;
    as2receiver1.LogDirectory := txtLogDir.Text;
  end;

end;
end;

procedure TFormAs2server.btnBrowseSignerCertClick(Sender: TObject);
begin
FormCertmgr.lblCaption.Caption := 'Select the sender''s public certificate used verify the signature.';
if (FormCertmgr.ShowModal = mrOK) then
begin
  as2receiver1.SignerCertStoreType := Tibeas2receiverCertStoreTypes(FormCertmgr.ibeCertMgr1.CertStoreType);
  as2receiver1.SignerCertStore := FormCertmgr.ibeCertMgr1.CertStore;
  as2receiver1.SignerCertStorePassword := FormCertmgr.ibeCertMgr1.CertStorePassword;
  as2receiver1.SignerCertSubject := FormCertmgr.ibeCertMgr1.CertSubject;
  txtSignerCert.Text := as2receiver1.SignerCertSubject;
end;
end;

procedure TFormAs2server.btnStartServerClick(Sender: TObject);
begin
  if(btnStartServer.Caption = '&Start Server') then
  begin
    IdHTTPServer1.DefaultPort := StrToInt(txtPort.Text);
    IdHTTPServer1.Active := True;
    Log('Server Started.');
    btnStartServer.Caption := '&Stop Server';
  end
  else
  begin
    IdHTTPServer1.Active := False;
    Log('Server Stopped.');
    btnStartServer.Caption := '&Start Server';
  end;
end;

procedure TFormAs2server.FormCreate(Sender: TObject);
begin
  mmoMessage.Lines.Clear;
  try
  as2receiver1.SignerCertStoreType := Tibeas2receiverCertStoreTypes.cstPEMKeyFile;
  as2receiver1.SignerCertStore := '..\\..\\as2sender.cer';
  as2receiver1.SignerCertSubject := '*';

  txtSignerCert.Text := as2receiver1.SignerCertSubject;

  as2receiver1.CertStoreType := Tibeas2receiverCertStoreTypes.cstPFXFile;
  as2receiver1.CertStore := '..\\..\\as2receiver.pfx';
  as2receiver1.CertStorePassword := 'test';
  as2receiver1.CertSubject := '*';

  as2receiver1.LogDirectory := txtLogDir.Text;

  txtDecryptionCert.Text := as2receiver1.CertSubject;
except on E: Exception do
  Log(E.Message);
end;

end;

Function StreamToRawByteString (F:TStream):RawByteString;
begin
  SetLength (Result, F.Size);
  F.Position := 0;
  F.Read (PAnsiChar (Result) ^, F.Size);
end;

procedure TFormAs2server.IdHTTPServer1CommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  m_RequestData: TBytes;
  m_RespHeaders: TStrings;
  m_RespHeaderDetails : TStrings;
  i : Integer;
begin

if (ARequestInfo.Command = 'POST') then
begin
    try
      Log('Received Request. Headers:' + #13#10 + ARequestInfo.RawHeaders.Text);

      AS2Receiver1.RequestHeadersString := ARequestInfo.RawHeaders.Text;
      SetLength(m_RequestData,ARequestInfo.PostStream.Size);
      ARequestInfo.PostStream.ReadBuffer(Pointer(m_RequestData)^,Length(m_RequestData));
      AS2Receiver1.RequestB := m_RequestData;

      //ReadRequest parses information about the request including the sender and recipient.
      //This data may be used to verify it is a known sender and load certificate information dynamically
      AS2Receiver1.ReadRequest;

      //Verify the message was sent to us.
      if(AS2Receiver1.AS2To <> txtAS2To.Text) then
      begin
        raise Exception.Create('This message is not meant for us(' + txtAS2To.Text +'). It is meant for \"' + AS2Receiver1.AS2To + '\"');
      end;

      //In a real application you would now load the certificate based on the AS2Receiver1.AS2From value
      //The AS2Receiver1.AS2From value identifies your partner who is sending the file.
      //Load the public certificate for the partner and assign it to the SignerCert property
      //  as2receiver1.SignerCertStoreType := Tibeas2receiverCertStoreTypes.cstPEMKeyFile;
      //  as2receiver1.SignerCertStore := '..\\..\\as2sender.cer';
      //  as2receiver1.SignerCertSubject := '*';
      //In this demo this is set ahead of time from.

      //Likewise you would need to specify your certificate with private key used for decryption, and
      //to sign the MDN that will be sent back. In this demo this is set ahead of time.

      //ProcessRequest will decrypt the incoming message and verify the signed data.
      //It will then create a MDN receipt (if request by your partner).
      //After calling this method the decrypted and verified EDI data is available.
      AS2Receiver1.ProcessRequest;
      Log('Request Processed Successfully. EDI Data:' + #13#10 + AS2Receiver1.EDIData);

      //If a synchronous MDN was requested it will be delivered in the HTTP response
      if(AS2Receiver1.ReceiptDeliveryOption = '') then
      begin
        for i := 0 to AS2Receiver1.MDNReceiptHeaderCount -1 do
        begin
          AS2Receiver1.MDNReceiptHeaderIndex := i;
          if(AS2Receiver1.MDNReceiptHeaderField = 'Content-Type') then
          begin
             AResponseInfo.ContentType := AS2Receiver1.MDNReceiptHeaderValue;
          end
          else
          begin
            AResponseInfo.CustomHeaders.AddValue(AS2Receiver1.MDNReceiptHeaderField,
              AS2Receiver1.MDNReceiptHeaderValue);
          end;
        end;
        AResponseInfo.ContentText := AS2Receiver1.MDNReceiptContent;
        Log('MDN Receipt Sent.');
      end;
    except
      on E: Exception do begin
        Log(E.Message);
        AResponseInfo.ContentText := E.Message;;
        AResponseInfo.ResponseNo := 400;
      end;
    end;

end
else

AResponseInfo.ContentText := '<html><head><title>IPWorks EDI - AS2Receiver Demo</title></head>' +
  '<body>This page is designed to accept post requests from an AS2 sender. Use the included AS2Sender demo to send a request to this page.' +
  '</body></html>';

begin
end;
end;

procedure TFormAs2server.Log(msg: String);
begin
  mmoMessage.Lines.Add('[' + TimeToStr(Now) +'] ' + msg);
end;

end.

