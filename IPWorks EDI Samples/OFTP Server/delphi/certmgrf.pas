unit certmgrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ibecore, ibetypes, ibecertmgr;

type
  TFormCertmgr = class(TForm)
    lblCaption: TLabel;
    ibeCertMgr1: TibeCertMgr;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    CertificateStore: TComboBox;
    Label4: TLabel;
    cboStoreTypes: TComboBox;
    btnBrowseCert: TButton;
    cmdOK: TButton;
    Label5: TLabel;
    txtCertFile: TEdit;
    Label6: TLabel;
    txtCertStorePassword: TEdit;
    GroupBox2: TGroupBox;
    AvailableCertificates: TListBox;
    btnListCertificates: TButton;
    Label2: TLabel;
    Label3: TLabel;
    CertificateInfo: TMemo;
    btnCancel: TButton;


    procedure ibeCertmgr1StoreList(Sender: TObject;
      const CertStore: String);
    procedure FormCreate(Sender: TObject);
    procedure AvailableCertificatesClick(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure ibeCertmgr1CertList(Sender: TObject; CertEncoded: string; CertEncodedB: TBytes;
      const CertSubject, CertIssuer, CertSerialNumber: String;
      HasPrivateKey: Boolean);
    procedure btnBrowseCertClick(Sender: TObject);
    procedure cboStoreTypesChange(Sender: TObject);
    procedure btnListCertificatesClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCertmgr: TFormCertmgr;

implementation

{$R *.DFM}






procedure TFormCertmgr.ibeCertmgr1StoreList(Sender: TObject;
  const CertStore: String);
begin
        CertificateStore.Items.Add(CertStore);
end;

procedure TFormCertmgr.FormCreate(Sender: TObject);
begin
  ibeCertMgr1.ListCertificateStores();

  cboStoreTypes.ItemIndex := 0;
  cboStoreTypesChange(nil);

  txtCertFile.Text := '..\..\oftpserver.pfx';
  txtCertStorePassword.Text := 'test';

end;

procedure TFormCertmgr.AvailableCertificatesClick(Sender: TObject);
begin
          ibeCertMgr1.CertSubject := AvailableCertificates.Items.Strings[AvailableCertificates.ItemIndex];
	CertificateInfo.Text :=
  	'Issuer: ' + ibeCertMgr1.CertIssuer + #13 + #10 +
		'Subject: ' + ibeCertMgr1.CertSubject + #13 + #10 +
		'Version: ' + ibeCertMgr1.CertVersion + #13 + #10 +
		'Serial Number: ' + ibeCertMgr1.CertSerialNumber + #13 + #10 +
		'Signature Algorithm: ' + ibeCertMgr1.CertSignatureAlgorithm + #13 + #10 +
		'Effective Date: ' + ibeCertMgr1.CertEffectiveDate + #13 + #10 +
		'Expiration Date: ' + ibeCertMgr1.CertExpirationDate + #13 + #10 +
		'Public Key Algorithm: ' + ibeCertMgr1.CertPublicKeyAlgorithm + #13 + #10 +
		'Public Key Length: ' + IntToStr(ibeCertMgr1.CertPublicKeyLength) + #13 + #10 +
		'Public Key: ' + ibeCertMgr1.CertPublicKey;
end;

procedure TFormCertmgr.btnBrowseCertClick(Sender: TObject);
var
  certFileDialog: TOpenDialog;
begin
  certFileDialog := TOpenDialog.Create(nil);
  try
    certFileDialog.InitialDir := '..\\..\\';
    certFileDialog.Filter := 'All Files (*.*)|*.*|Public Certs (*.cer,*.crt)|*.CER;*.CRT|PFX Files (*.pfx,*.p12)|*.PFX;*.P12';
    if(certFileDialog.Execute(Handle)) then
      txtCertFile.Text := certFileDialog.FileName;
  finally
    certFileDialog.Free;
  end;


end;

procedure TFormCertmgr.btnListCertificatesClick(Sender: TObject);
begin
    AvailableCertificates.Clear;
    CertificateInfo.Clear;

    case cboStoreTypes.ItemIndex of
      0 : begin
        ibeCertMgr1.CertStoreType := TibecertmgrCertStoreTypes.cstPFXFile;
        ibeCertMgr1.CertStorePassword := txtCertStorePassword.Text;
        ibeCertMgr1.CertStore := txtCertFile.Text;
      end;
      1 : begin
        ibeCertMgr1.CertStoreType := TibecertmgrCertStoreTypes.cstPEMKeyFile;
        ibeCertMgr1.CertStore := txtCertFile.Text;
      end
      else begin
        ibeCertMgr1.CertStoreType := TibecertmgrCertStoreTypes.cstUser;
        ibeCertMgr1.CertStore := CertificateStore.Items.Strings[CertificateStore.ItemIndex];
      end;
    end;

    ibeCertMgr1.ListStoreCertificates();
    if AvailableCertificates.Items.Count > 0 then
  	begin
  		AvailableCertificates.ItemIndex := 0;
	  	AvailableCertificatesClick(nil);
    end;
end;



procedure TFormCertmgr.cboStoreTypesChange(Sender: TObject);
begin

    txtCertFile.Text := '';
    txtCertStorePassword.Text := '';

    if(cboStoreTypes.ItemIndex = 2) then //Windows store
    begin
      txtCertFile.Enabled := False;
      txtCertStorePassword.Enabled := False;
      btnBrowseCert.Enabled :=False;
      CertificateStore.Enabled := True;

      if CertificateStore.Items.Count > 0 then
      begin
  	    CertificateStore.ItemIndex := 0;
    	  btnListCertificatesClick(nil);
      end;
    end
    else //File based
    begin
      txtCertFile.Enabled := True;
      txtCertStorePassword.Enabled := True;
      btnBrowseCert.Enabled :=True;
      CertificateStore.Enabled := False;
    end;
end;

procedure TFormCertmgr.cmdOKClick(Sender: TObject);
begin
ModalResult := mrOK;
end;

procedure TFormCertmgr.btnCancelClick(Sender: TObject);
begin
ModalResult := mrCancel;
end;

procedure TFormCertmgr.ibeCertmgr1CertList(Sender: TObject;
  CertEncoded: string; CertEncodedB: TBytes; const CertSubject, CertIssuer,
  CertSerialNumber: String; HasPrivateKey: Boolean);
begin
     AvailableCertificates.Items.Add(CertSubject);
end;

end.
