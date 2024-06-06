unit GenCertDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, StrUtils, ibecore, ibetypes,
  ibecertmgr;

type
  TFormgencertdl = class(TForm)
    CertDetailGroupBox: TGroupBox;
    lblNewCertDetails: TLabel;
    lblNewCertSerial: TLabel;
    lblNewCertExp: TLabel;
    lblNewCertPass: TLabel;
    txtNewCertSubject: TEdit;
    txtNewCertPass: TEdit;
    txtNewCertExp: TEdit;
    txtNewCertSerial: TEdit;
    txtNewCertSerialUpDown: TUpDown;
    txtNewCertExpUpDown: TUpDown;
    btnNewCertCancel: TButton;
    btnNewCertGenerate: TButton;
    ibeCertMgr1: TibeCertMgr;
    procedure btnNewCertGenerateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Formgencertdl: TFormgencertdl;
  fileName: string;

implementation

{$R *.dfm}

procedure TFormgencertdl.btnNewCertGenerateClick(Sender: TObject);
var
  validityTime: Integer;
  openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(self);
  openDialog.InitialDir := GetCurrentDir();
  openDialog.Filter := 'Putty Private Keys|*.pfx';
  if openDialog.Execute then
  begin
    fileName := openDialog.FileName;
  end
  else
  begin
    showMessage('Please select a filename.');
  end;
  openDialog.Free;

  if not EndsStr('.pfx', fileName) then
  begin
    fileName := fileName + '.pfx';
  end;

  try
    validityTime := 365 * strtoint(txtNewCertExp.Text);
    ibeCertMgr1.Config('CertValidityTime=' + inttostr(validityTime));
    ibeCertMgr1.CreateCertificate('CN=' + txtNewCertSubject.Text, strtoint(txtNewCertSerial.Text));
    ibeCertMgr1.ExportCertificate(fileName, txtNewCertPass.Text);
    ibeCertMgr1.DeleteCertificate();
  except on E: EIPWorksEDI do
    ShowMessage('Could not generate new certificate: ' + E.Message);
  end;

end;

end.
