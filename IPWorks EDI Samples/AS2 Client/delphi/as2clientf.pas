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
unit as2clientf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ibecore, ibetypes, StrUtils, certmgrf,
  ibecertmgr, ibeas2sender;

type
  TFormAs2client = class(TForm)
    Label1: TLabel;
    as2sender1: TibeAS2Sender;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    txtAs2From: TEdit;
    txtAs2To: TEdit;
    txtURL: TEdit;
    txtLogDir: TEdit;
    btnBrowseLogDir: TButton;
    chkSigned: TCheckBox;
    chkEncrypted: TCheckBox;
    chkCompressed: TCheckBox;
    chkRequestSignature: TCheckBox;
    btnSend: TButton;
    txtEDIData: TMemo;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    txtHeaders: TMemo;
    Label7: TLabel;
    txtReceipt: TMemo;
    Label8: TLabel;
    txtReceiptMessage: TMemo;
    txtMDN: TMemo;
    Label9: TLabel;
    procedure btnBrowseLogDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkSignedClick(Sender: TObject);
    procedure chkEncryptedClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAs2client: TFormAs2client;

implementation

{$R *.dfm}

procedure TFormAs2client.FormCreate(Sender: TObject);
begin
  as2sender1.LogDirectory := txtLogDir.Text;
end;

procedure TFormAs2client.btnBrowseLogDirClick(Sender: TObject);
begin
with TFileOpenDialog.Create(nil) do
begin
  Options := [fdoPickFolders];
  if Execute then begin
    txtLogDir.Text := FileName;
    as2sender1.LogDirectory := txtLogDir.Text;
  end;
end;
end;

procedure TFormAs2client.chkSignedClick(Sender: TObject);
begin
    if (chkSigned.Checked) then
    begin
      FormCertmgr.lblCaption.Caption := 'Select a private key used to sign outgoing data, and decrypt MDNs.';
      FormCertmgr.cboStoreTypes.ItemIndex := 0;
      FormCertmgr.txtCertFile.Text := '..\..\as2sender.pfx';
      FormCertmgr.txtCertStorePassword.Text := 'test';
      if (FormCertmgr.ShowModal = mrOK) then
      begin
        as2sender1.SigningCertStoreType := Tibeas2senderSigningCertStoreTypes(FormCertmgr.ibeCertMgr1.CertStoreType);
        as2sender1.SigningCertStore := FormCertmgr.ibeCertMgr1.CertStore;
        as2sender1.SigningCertStorePassword := FormCertmgr.ibeCertMgr1.CertStorePassword;
        as2sender1.SigningCertSubject := FormCertmgr.ibeCertMgr1.CertSubject;
      end else begin
        chkSigned.Checked := False;
      end;
    end else begin
       as2sender1.SigningCertStore := '';
       as2sender1.SigningCertStorePassword := '';
       as2sender1.SigningCertSubject := '';
    end;
end;

procedure TFormAs2client.chkEncryptedClick(Sender: TObject);
begin
    if (chkEncrypted.Checked) then
    begin
      FormCertmgr.lblCaption.Caption := 'Select the sender''s public certificate used to encrypt outgoing data.';
      FormCertmgr.cboStoreTypes.ItemIndex := 1;
      FormCertmgr.txtCertFile.Text := '..\..\as2receiver.cer';
      FormCertmgr.txtCertStorePassword.Text := '';
      if (FormCertmgr.ShowModal = mrOK) then
      begin
        as2sender1.RecipientCertCount := 1;
        as2sender1.RecipientCertStoreType[0] := Tibeas2senderRecipientCertStoreTypes(FormCertmgr.ibeCertMgr1.CertStoreType);
        as2sender1.RecipientCertStore[0] := FormCertmgr.ibeCertMgr1.CertStore;
        as2sender1.RecipientCertStorePassword[0] := FormCertmgr.ibeCertMgr1.CertStorePassword;
        as2sender1.RecipientCertSubject[0] := FormCertmgr.ibeCertMgr1.CertSubject;
      end else begin
        chkEncrypted.Checked := False;
      end;
    end else begin
      as2sender1.RecipientCertCount := 0;
    end;
end;

procedure TFormAs2client.btnSendClick(Sender: TObject);
begin
    // The first thing to do is specify the necessary AS2 identifiers.
    as2sender1.AS2From := txtAs2From.Text;
    as2sender1.AS2To := txtAs2To.Text;

    // You also need to set the certificates for yourself and your trading
    // partner. See the events below for an illustration of how this is done.

    // To request an MDN (Message Disposition Notification) based receipt, you
    // should set the MDNTo property. By default, the component will request a
    // SIGNED receipt, with a Received-Content-MID value that establishes
    // digital non-repudiation. If you prefer to receive an unsigned receipt,
    // you should set MDNOptions to an empty string.

    as2sender1.MDNTo := 'as2@nsoftware.com'; // Note: the actual value is
    // irrelevant; most servers just check that something is specified.

    if (Not chkRequestSignature.Checked) then
    begin
      as2sender1.MDNOptions := '';
    end;

    // By default, the component will request that the receipt be delivered
    // synchronously over the same HTTP connection. If you prefer to receive
    // your receipt asynchronously, you should set MDNDeliveryOption and provide
    // additional processing for inbound asynchronous receipts.

    // if (chkAsyncMDN.Checked) then
    // begin
    //   as2sender1.MDNDeliveryOption := 'http://localhost:8000/AsyncMDN';
    //   // This will instruct the component to preserve the details of the transmission
    //   // on disk, so that when the AsyncMDN arrives, it can be matched
    //	 as2.AsyncMDNInfoDir = Path.Combine(Application.StartupPath, "..\\..\\..\\..\\..\\demos - webform\\aspx2-cs\\MDNInfo");
    // end;

    // Outgoing data may also be compressed.
    if (chkCompressed.Checked) then
    begin
      as2sender1.CompressionFormat := Tibeas2senderCompressionFormats.cfZLIB;
    end else begin
      as2sender1.CompressionFormat := Tibeas2senderCompressionFormats.cfNone;
    end;

    // If you set a log directory, the component will product detailed log files.
    as2sender1.LogDirectory := txtLogDir.Text;
    as2sender1.URL := txtURL.Text;

    as2sender1.EDIType := 'application/edi-x12';
    as2sender1.EDIData := txtEDIData.Text;

    try
      Cursor := crHourGlass;
      as2sender1.Post();

  		// If the call to post returns without throwing an exception, then the
			// component was able to post the data and verify the response. In particular,
			// if you requested a synchronous MDN, it will automatically be validated,
			// and an exception will be thrown if there are any problems.

      // If you requested an asynchronous MDN, you will need to save the values
			// of MessageId, OriginalContentMIC, and MDNOptions, so they can be looked
			// up based on the MessageId (this is set in the code above when the
			// AsyncMDNInfoDir property is set). Then, when you process the asynchronous MDN,
			// you will need to load these values into the component to verify the
      // MDN.

      ShowMessage('Transmission was successful and the MDN has been verified.');

    except on E: Exception do
      ShowMessage('Transmission was unsuccessful: ' + E.Message);
    end;

    Cursor := crDefault;
    if (as2sender1.MDNReceiptMDN <> '') then
    begin    
      txtHeaders.Text := as2sender1.MDNReceiptHeaders;
      txtReceipt.Text := as2sender1.MDNReceiptContent;
      txtReceiptMessage.Text := as2sender1.MDNReceiptMessage;
      txtMDN.Text := as2sender1.MDNReceiptMDN;
    end;
end;

end.


