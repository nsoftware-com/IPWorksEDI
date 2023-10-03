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
unit x12writerf;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ibecore, ibex12writer;

type
  TFormX12writer = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cbFileType: TComboBox;
    btnWriteX12: TButton;
    Label4: TLabel;
    txtLog: TMemo;
    Label5: TLabel;
    txtFile: TMemo;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
//    ibeX12Writer1: TibeX12Writer;
    ibeX12Writer1: TibeX12Writer;
    procedure cbStandardChange(Sender: TObject);
    procedure btnWriteX12Click(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure writeFile();
    procedure writeFile_X12_810();
    procedure writeFile_X12_850();
    procedure writeFile_X12_855();
    procedure writeFile_X12_856();
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormX12writer: TFormX12writer;

implementation

{$R *.dfm}

procedure TFormX12writer.btnSaveClick(Sender: TObject);
var
  dir: AnsiString;
begin
  SaveDialog1.Filter := 'All Files (*.*)|*.*';
  dir := GetCurrentDir;
  if (SaveDialog1.Execute() = True) then
    // write to file
    ibeX12Writer1.OutputFile := SaveDialog1.FileName;
  ibeX12Writer1.FileWriteMode := TibeX12WriterFileWriteModes.fwmOverwrite;
  writeFile();
  ibeX12Writer1.OutputFile := '';
  SetCurrentDir(dir);
end;

procedure TFormX12writer.writeFile();
begin
  txtLog.Text := '';

    if (cbFileType.ItemIndex = 0) then // 810
    begin
      writeFile_X12_810();
    end
    else if (cbFileType.ItemIndex = 1) then // 850
    begin
      writeFile_X12_850();
    end
    else if (cbFileType.ItemIndex = 2) then // 855
    begin
      writeFile_X12_855();
    end
    else if (cbFileType.ItemIndex = 3) then // 856
    begin
      writeFile_X12_856();
    end;

  txtLog.Lines.Add('Write Complete' + #13#10);

  txtFile.Text := ibeX12Writer1.OutputData;
end;

procedure TFormX12writer.writeFile_X12_810();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  ibeX12Writer1.StartInterchangeHeader('004010');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('U');
  ibeX12Writer1.WriteElementString('00401');
  ibeX12Writer1.WriteElementString('000000006');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('>');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  ibeX12Writer1.StartFunctionalGroupHeader();
  ibeX12Writer1.WriteElementString('IN');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('20160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('6');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('004010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  ibeX12Writer1.StartTransactionHeader('810');
  ibeX12Writer1.WriteElementString('810');
  ibeX12Writer1.WriteElementString('0001');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BIG');
  ibeX12Writer1.StartSegment('BIG');
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.WriteElementString('3003014445');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('0476553272');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('DR');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CUR');
  ibeX12Writer1.StartSegment('CUR');
  ibeX12Writer1.WriteElementString('SE');
  ibeX12Writer1.WriteElementString('USD');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('8M');
  ibeX12Writer1.WriteElementString('0056');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('BY');
  ibeX12Writer1.WriteElementString('Company');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('544380');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  ibeX12Writer1.StartSegment('N1Loop1/N3');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  ibeX12Writer1.StartSegment('N1Loop1/N4');
  ibeX12Writer1.WriteElementString('City');
  ibeX12Writer1.WriteElementString('CA');
  ibeX12Writer1.WriteElementString('Postal Code');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('ST');
  ibeX12Writer1.WriteElementString('Name');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('0607047800010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  ibeX12Writer1.StartSegment('N1Loop1/N3');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  ibeX12Writer1.StartSegment('N1Loop1/N4');
  ibeX12Writer1.WriteElementString('City');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('200131');
  ibeX12Writer1.WriteElementString('Country');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('RE');
  ibeX12Writer1.WriteElementString('Name');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('5095956');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  ibeX12Writer1.StartSegment('N1Loop1/N3');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  ibeX12Writer1.StartSegment('N1Loop1/N4');
  ibeX12Writer1.WriteElementString('City');
  ibeX12Writer1.WriteElementString('IL');
  ibeX12Writer1.WriteElementString('Postal Code');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: IT1Loop1');
  txtLog.Lines.Add('Segment: IT1');
  ibeX12Writer1.StartSegment('IT1Loop1/IT1');
  ibeX12Writer1.WriteElementString('20');
  ibeX12Writer1.WriteElementString('2500');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.WriteElementString('36.96');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('BP');
  ibeX12Writer1.WriteElementString('335S0594');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_3');
  ibeX12Writer1.StartSegment('IT1Loop1/REF_3');
  ibeX12Writer1.WriteElementString('KK');
  ibeX12Writer1.WriteElementString('0099778154');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_3\');
  ibeX12Writer1.StartSegment('IT1Loop1/REF_3');
  ibeX12Writer1.WriteElementString('PO');
  ibeX12Writer1.WriteElementString('0476553272');
  ibeX12Writer1.WriteElementString('20');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: TDS');
  ibeX12Writer1.StartSegment('TDS');
  ibeX12Writer1.WriteElementString('9240000');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CTT');
  ibeX12Writer1.StartSegment('CTT');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  ibeX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  ibeX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  ibeX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_850();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  ibeX12Writer1.StartInterchangeHeader('004010');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('U');
  ibeX12Writer1.WriteElementString('00401');
  ibeX12Writer1.WriteElementString('000000007');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('>');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  ibeX12Writer1.StartFunctionalGroupHeader();
  ibeX12Writer1.WriteElementString('PO');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('20160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('7');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('004010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  ibeX12Writer1.StartTransactionHeader('850');
  ibeX12Writer1.WriteElementString('850');
  ibeX12Writer1.WriteElementString('0001');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BEG');
  ibeX12Writer1.StartSegment('BEG');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.WriteElementString('DS');
  ibeX12Writer1.WriteElementString('0476696888');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('SB');
  ibeX12Writer1.WriteElementString('ZZ11');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('6P');
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('8M');
  ibeX12Writer1.WriteElementString('0056');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('CR');
  ibeX12Writer1.WriteElementString('1070335099');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('REF');
  ibeX12Writer1.WriteElementString('CO');
  ibeX12Writer1.WriteElementString('7109790082');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PER');
  ibeX12Writer1.StartSegment('PER');
  ibeX12Writer1.WriteElementString('CN');
  ibeX12Writer1.WriteElementString('name');
  ibeX12Writer1.WriteElementString('TE');
  ibeX12Writer1.WriteElementString('Number');

  txtLog.Lines.Add('Segment: CSH');
  ibeX12Writer1.StartSegment('CSH');
  ibeX12Writer1.WriteElementString('BK');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: SACLoop1');
  txtLog.Lines.Add('Segment: SAC');
  ibeX12Writer1.StartSegment('SACLoop1/SAC');
  ibeX12Writer1.WriteElementString('C');
  ibeX12Writer1.WriteElementString('ZZZZ');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('06');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: TD5');
  ibeX12Writer1.StartSegment('TD5');
  ibeX12Writer1.WriteElementString('Z');
  ibeX12Writer1.WriteElementString('2');
  ibeX12Writer1.WriteElementString('Code');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N9Loop1');
  txtLog.Lines.Add('Segment: N9');
  ibeX12Writer1.StartSegment('N9Loop1/N9');
  ibeX12Writer1.WriteElementString('PD');
  ibeX12Writer1.WriteElementString('ZCOF');
  ibeX12Writer1.EndElement();

  ibeX12Writer1.StartSegment('N9Loop1/MSG');
  ibeX12Writer1.WriteElementString('Thanks!');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1n');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('BY');
  ibeX12Writer1.WriteElementString('Name');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('5601');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('EN');
  ibeX12Writer1.WriteElementString('Name');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('N1Loop1/N1');
  ibeX12Writer1.WriteElementString('ST');
  ibeX12Writer1.WriteElementString('OEM NAME');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('0000505462');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N2');
  ibeX12Writer1.StartSegment('N1Loop1/N2');
  ibeX12Writer1.WriteElementString('additional name');
  ibeX12Writer1.WriteElementString(''); // not skipped because last element
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  ibeX12Writer1.StartSegment('N1Loop1/N3');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  ibeX12Writer1.StartSegment('N1Loop1/N4');
  ibeX12Writer1.WriteElementString('City');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('201613');
  ibeX12Writer1.WriteElementString('CN');
  ibeX12Writer1.WriteElementString('SP');
  ibeX12Writer1.WriteElementString('020');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: PO1Loop1');
  txtLog.Lines.Add('Segment: PO1');
  ibeX12Writer1.StartSegment('PO1Loop1/PO1');
  ibeX12Writer1.WriteElementString('00010');
  ibeX12Writer1.WriteElementString('500000');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.WriteElementString('495');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('BP');
  ibeX12Writer1.WriteElementString('337S3744');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PID_2');
  ibeX12Writer1.StartSegment('PO1Loop1/PIDLoop1/PID_2');
  ibeX12Writer1.WriteElementString('F');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('Thanks!');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('EN');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('Segment: REF_7');
  ibeX12Writer1.StartSegment('PO1Loop1/REF_7');
  ibeX12Writer1.WriteElementString('CO');
  ibeX12Writer1.WriteElementString('7109790082');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF_7');
  ibeX12Writer1.StartSegment('PO1Loop1/REF_7');
  ibeX12Writer1.WriteElementString('LI');
  ibeX12Writer1.WriteElementString('000010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: SCHLoop1');
  txtLog.Lines.Add('Segment: SCH');
  ibeX12Writer1.StartSegment('PO1Loop1/SCHLoop1/SCH');
  ibeX12Writer1.WriteElementString('500000');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('002');
  ibeX12Writer1.WriteElementString('20180708');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('StartLoop: CTTLoop1');
  txtLog.Lines.Add('Segment: CTT');
  ibeX12Writer1.StartSegment('CTTLoop1/CTT');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.WriteElementString('500000');
  ibeX12Writer1.EndElement();
  txtLog.Lines.Add('EndLoop');

  txtLog.Lines.Add('EndTransaction');
  ibeX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  ibeX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  ibeX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_855();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  ibeX12Writer1.StartInterchangeHeader('004010');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('U');
  ibeX12Writer1.WriteElementString('00401');
  ibeX12Writer1.WriteElementString('000000008');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('>');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  ibeX12Writer1.StartFunctionalGroupHeader();
  ibeX12Writer1.WriteElementString('PR');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('20160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('8');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('004010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  ibeX12Writer1.StartTransactionHeader('855');
  ibeX12Writer1.WriteElementString('855');
  ibeX12Writer1.WriteElementString('0013');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BAK');
  ibeX12Writer1.StartSegment('BAK');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.WriteElementString('AT');
  ibeX12Writer1.WriteElementString('0476553696');
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('4900043704');
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: PO1Loop1');
  txtLog.Lines.Add('Segment: PO1');
  ibeX12Writer1.StartSegment('PO1Loop1/PO1');
  ibeX12Writer1.WriteElementString('000010');
  ibeX12Writer1.WriteElementString('1100');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.WriteElementString('14.00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('BP');
  ibeX12Writer1.WriteElementString('335S0548');
  ibeX12Writer1.WriteElementString('VP');
  ibeX12Writer1.WriteElementString('Product');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('PO1Loop1/REF');
  ibeX12Writer1.WriteElementString('PO');
  ibeX12Writer1.WriteElementString('0476553696');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('PO1Loop1/REF');
  ibeX12Writer1.WriteElementString('VN');
  ibeX12Writer1.WriteElementString('0025009879');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: ACKLoop1');
  txtLog.Lines.Add('Segment: ACK');
  ibeX12Writer1.StartSegment('PO1Loop1/ACKLoop1/ACK');
  ibeX12Writer1.WriteElementString('IA');
  ibeX12Writer1.WriteElementString('1100');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.WriteElementString('067');
  ibeX12Writer1.WriteElementString('20150709');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: CTTLoop1');
  txtLog.Lines.Add('Segment: CTT');
  ibeX12Writer1.StartSegment('CTTLoop1/CTT');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.WriteElementString('1100');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  ibeX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  ibeX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  ibeX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.writeFile_X12_856();
begin
  txtLog.Lines.Add('StartInterchangeHeader');
  ibeX12Writer1.StartInterchangeHeader('004010');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('ZZ');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('U');
  ibeX12Writer1.WriteElementString('00401');
  ibeX12Writer1.WriteElementString('000000009');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('>');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartFunctionalGroupHeader');
  ibeX12Writer1.StartFunctionalGroupHeader();
  ibeX12Writer1.WriteElementString('SH');
  ibeX12Writer1.WriteElementString('ACME');
  ibeX12Writer1.WriteElementString('WAYNE_TECH');
  ibeX12Writer1.WriteElementString('20160707');
  ibeX12Writer1.WriteElementString('1544');
  ibeX12Writer1.WriteElementString('9');
  ibeX12Writer1.WriteElementString('T');
  ibeX12Writer1.WriteElementString('004010');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartTransactionHeader');
  ibeX12Writer1.StartTransactionHeader('856');
  ibeX12Writer1.WriteElementString('856');
  ibeX12Writer1.WriteElementString('0029');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: BSN');
  ibeX12Writer1.StartSegment('BSN');
  ibeX12Writer1.WriteElementString('00');
  ibeX12Writer1.WriteElementString('0403734501');
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.WriteElementString('162859');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: DTM');
  ibeX12Writer1.StartSegment('DTM');
  ibeX12Writer1.WriteElementString('011');
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  ibeX12Writer1.StartSegment('HLLoop1/HL');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('S');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: PRF');
  ibeX12Writer1.StartSegment('HLLoop1/PRF');
  ibeX12Writer1.WriteElementString('0476553696');
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.SkipElement();
  ibeX12Writer1.WriteElementString('20150708');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: TD1');
  ibeX12Writer1.StartSegment('HLLoop1/TD1');
  ibeX12Writer1.WriteElementString('CNT90');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: TD5');
  ibeX12Writer1.StartSegment('HLLoop1/TD5');
  ibeX12Writer1.WriteElementString('O');
  ibeX12Writer1.WriteElementString('2');
  ibeX12Writer1.WriteElementString('FEDX');
  ibeX12Writer1.WriteElementString('A');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('HLLoop1/REF');
  ibeX12Writer1.WriteElementString('BM');
  ibeX12Writer1.WriteElementString('EDITEST403734501');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: REF');
  ibeX12Writer1.StartSegment('HLLoop1/REF');
  ibeX12Writer1.WriteElementString('CR');
  ibeX12Writer1.WriteElementString('4900043704');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  ibeX12Writer1.StartSegment('HLLoop1/HL');
  ibeX12Writer1.WriteElementString('2');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.WriteElementString('O');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: N1Loop1');
  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('HLLoop1/N1Loop1/N1');
  ibeX12Writer1.WriteElementString('ST');
  ibeX12Writer1.WriteElementString('Name');
  ibeX12Writer1.WriteElementString('92');
  ibeX12Writer1.WriteElementString('0042001808');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N1');
  ibeX12Writer1.StartSegment('HLLoop1/N1Loop1/N1');
  ibeX12Writer1.WriteElementString('SF');
  ibeX12Writer1.WriteElementString('NameT');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N3');
  ibeX12Writer1.StartSegment('HLLoop1/N1Loop1/N3');
  ibeX12Writer1.WriteElementString('Address');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: N4');
  ibeX12Writer1.StartSegment('HLLoop1/N1Loop1/N4');
  ibeX12Writer1.WriteElementString('City');
  ibeX12Writer1.WriteElementString('SG');
  ibeX12Writer1.WriteElementString('339942');
  ibeX12Writer1.WriteElementString('SG');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('StartLoop: HLLoop1');
  txtLog.Lines.Add('Segment: HL');
  ibeX12Writer1.StartSegment('HLLoop1/HL');
  ibeX12Writer1.WriteElementString('3');
  ibeX12Writer1.WriteElementString('2');
  ibeX12Writer1.WriteElementString('I');
  ibeX12Writer1.WriteElementString('0');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: LIN');
  ibeX12Writer1.StartSegment('HLLoop1/LIN');
  ibeX12Writer1.WriteElementString('10');
  ibeX12Writer1.WriteElementString('BP');
  ibeX12Writer1.WriteElementString('335S0548');
  ibeX12Writer1.WriteElementString('VP');
  ibeX12Writer1.WriteElementString('Product');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: SN1');
  ibeX12Writer1.StartSegment('HLLoop1/SN1');
  ibeX12Writer1.WriteElementString('10');
  ibeX12Writer1.WriteElementString('1100');
  ibeX12Writer1.WriteElementString('EA');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: MAN');
  ibeX12Writer1.StartSegment('HLLoop1/MAN');
  ibeX12Writer1.WriteElementString('CP');
  ibeX12Writer1.WriteElementString('Marks');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('Segment: CTT');
  ibeX12Writer1.StartSegment('CTT');
  ibeX12Writer1.WriteElementString('1');
  ibeX12Writer1.EndElement();

  txtLog.Lines.Add('EndTransaction');
  ibeX12Writer1.CreateTransactionFooter();
  txtLog.Lines.Add('EndFunctionalGroup');
  ibeX12Writer1.CreateFunctionalGroupFooter();
  txtLog.Lines.Add('EndInterchange');
  ibeX12Writer1.CreateInterchangeFooter();
end;

procedure TFormX12writer.btnWriteX12Click(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    try
      ibeX12Writer1.Reset();
      txtLog.Text := '';
      txtFile.Text := '';

      // load proper schema for file type
      ibeX12Writer1.LoadSchema('RSSBus_00401_' + cbFileType.Text + '.json');

      ibeX12Writer1.Suffix := TibeX12WriterSuffixes(3); // suffixCRLF

      ibeX12Writer1.Config('Encoding=iso-8859-1');
      writeFile();
      Screen.Cursor := crDefault;
    except
      on E: Exception do
        MessageDlg('Exception ' + E.Message, mtInformation, [mbOk], 0);
    end;
  finally
    Screen.Cursor := crDefault
  end;

end;

procedure TFormX12writer.cbStandardChange(Sender: TObject);
begin

  cbFileType.Items.Clear();
  cbFileType.AddItem('810', nil);
  cbFileType.AddItem('850', nil);
  cbFileType.AddItem('855', nil);
  cbFileType.AddItem('856', nil);
  cbFileType.ItemIndex := 0;

end;

procedure TFormX12writer.FormCreate(Sender: TObject);
begin
  cbFileType.ItemIndex := 0;
end;

end.

