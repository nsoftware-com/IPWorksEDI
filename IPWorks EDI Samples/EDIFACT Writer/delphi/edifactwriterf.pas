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
unit edifactwriterf;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ibecore, ibeedifactwriter, ibetypes;

type
    TFormEdifactwriter = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    cbFileType: TComboBox;
    btnWriteEDI: TButton;
    Label4: TLabel;
    txtLog: TMemo;
    Label5: TLabel;
    txtFile: TMemo;
    btnSave: TButton;
    SaveDialog1: TSaveDialog;
    ibeEDIFACTWriter1: TibeEDIFACTWriter;
    procedure btnWriteEDIClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure writeFile();
    procedure writeFile_EDIFACT_DESADV();
    procedure writeFile_EDIFACT_INVOIC();
    procedure writeFile_EDIFACT_ORDERS();
    procedure writeFile_EDIFACT_ORDRSP();
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormEdifactwriter: TFormEdifactwriter;

implementation

{$R *.dfm}

procedure TFormEdifactwriter.btnSaveClick(Sender: TObject);
var
dir : AnsiString;
begin
  SaveDialog1.Filter := 'All Files (*.*)|*.*';
  dir:=GetCurrentDir;
  if (SaveDialog1.Execute() = True) then
    // write to file
    ibeEDIFACTWriter1.OutputFile := SaveDialog1.FileName;
    ibeEDIFACTWriter1.FileWriteMode := TibeEDIFACTWriterFileWriteModes(fwmOverwrite);
  writeFile();
  ibeEDIFACTWriter1.OutputFile := '';
  SetCurrentDir(dir) ;
end;

procedure TFormEdifactwriter.writeFile();
begin
  txtLog.Text := '';

  if (cbFileType.Text = 'DESADV') then
  begin
    writeFile_EDIFACT_DESADV();
  end
  else if (cbFileType.Text = 'INVOIC') then
  begin
    writeFile_EDIFACT_INVOIC();
  end
  else if (cbFileType.Text = 'ORDERS') then
  begin
    writeFile_EDIFACT_ORDERS();
  end
  else if (cbFileType.Text = 'ORDRSP') then
  begin
    writeFile_EDIFACT_ORDRSP();
  end;

  txtLog.Text := txtLog.Text + #13#10 + #13#10 + #13#10 +
    'Write Complete' + #13#10;

  txtFile.Text := ibeEDIFACTWriter1.OutputData;
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_DESADV();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  ibeEDIFACTWriter1.StartInterchangeHeader('D97A');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('UNOB');
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ACME');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('160707');
  ibeEDIFACTWriter1.WriteComponentString('1547');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('000000001');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('1234');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  ibeEDIFACTWriter1.StartTransactionHeader('DESADV');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('DESADV');
  ibeEDIFACTWriter1.WriteComponentString('D');
  ibeEDIFACTWriter1.WriteComponentString('97 A');
  ibeEDIFACTWriter1.WriteComponentString('UN');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('BGM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('351');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2014/10093');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('9');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('137');
  ibeEDIFACTWriter1.WriteComponentString('201404192036');
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('201404192036');
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MEA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MEA');
  ibeEDIFACTWriter1.WriteElementString('AAX');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('SQ');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('C62');
  ibeEDIFACTWriter1.WriteComponentString('17');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('ST');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0018');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('SU');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2019813');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TDTLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TDT' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TDTLoop1/TDT');
  ibeEDIFACTWriter1.WriteElementString('12');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('M');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('CARRIER');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('86');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: EQDLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: EQD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('EQDLoop1/EQD');
  ibeEDIFACTWriter1.WriteElementString('TE');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('X');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CPSLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CPS' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/CPS');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PACLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/PAC');
  ibeEDIFACTWriter1.WriteElementString('4');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('BOX - 001');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('52');
  ibeEDIFACTWriter1.WriteComponentString('50');
  ibeEDIFACTWriter1.WriteComponentString('C62');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CPSLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CPS' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/CPS');
  ibeEDIFACTWriter1.WriteElementString('2');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PACLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/PAC');
  ibeEDIFACTWriter1.WriteElementString('2');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('BOX - 002');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/PACLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('52');
  ibeEDIFACTWriter1.WriteComponentString('100');
  ibeEDIFACTWriter1.WriteComponentString('C62');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('9001');
  ibeEDIFACTWriter1.WriteComponentString('IN');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CPSLoop1/LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('12');
  ibeEDIFACTWriter1.WriteComponentString('400');
  ibeEDIFACTWriter1.WriteComponentString('C62');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ON');
  ibeEDIFACTWriter1.WriteComponentString('N55109001');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  ibeEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  ibeEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_INVOIC();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  ibeEDIFACTWriter1.StartInterchangeHeader('D97A');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('UNOB');
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ACME');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('160707');
  ibeEDIFACTWriter1.WriteComponentString('1547');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('000000002');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('1234');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  ibeEDIFACTWriter1.StartTransactionHeader('INVOIC');
  ibeEDIFACTWriter1.WriteElementString('509010117');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INVOIC');
  ibeEDIFACTWriter1.WriteComponentString('D');
  ibeEDIFACTWriter1.WriteComponentString('97A');
  ibeEDIFACTWriter1.WriteComponentString('UN');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('BGM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('380');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('TAX INVOICE');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0013550417');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('9');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('3');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('4');
  ibeEDIFACTWriter1.WriteComponentString('20061123');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: FTX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('FTX');
  ibeEDIFACTWriter1.WriteElementString('AAI');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('10072.14');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('EUR');
  ibeEDIFACTWriter1.WriteComponentString('4');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('0.67529');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PATLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PAT' + #13#10;
  ibeEDIFACTWriter1.StartSegment('PATLoop1/PAT');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('PATLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('10');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PCD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('PATLoop1/PCD');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('0');
  ibeEDIFACTWriter1.WriteComponentString('13');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000030');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2265S13');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5029766832002');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('50.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('19150.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('383.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('19150.45');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.45');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.45');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000040');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2269F22');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5051254078241');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('20.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('21060.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('1053.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('21060.50');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.50');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.50');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000170');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2269F10');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5051254078326');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('10.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('6950.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('695.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('6950.16');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.16');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.16');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000190');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2269F26');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5051254051190');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('5.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('2375.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('475.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('2375.06');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.06');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.06');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000200');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2265S24');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5029766000685');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('3.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('957.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('319.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('957.02');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.02');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.02');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000210');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2263T95');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5029766699575');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('3.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('2085.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('695.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('2085.05');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.05');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.05');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('000250');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2269F15');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('5051254080091');
  ibeEDIFACTWriter1.WriteComponentString('UP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('47');
  ibeEDIFACTWriter1.WriteComponentString('3.000');
  ibeEDIFACTWriter1.WriteComponentString('EA');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('11');
  ibeEDIFACTWriter1.WriteComponentString('20070926');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('203');
  ibeEDIFACTWriter1.WriteComponentString('4977.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INV');
  ibeEDIFACTWriter1.WriteComponentString('1659.00');
  ibeEDIFACTWriter1.WriteComponentString('TU');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('4977.12');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0.12');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('0.12');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  ibeEDIFACTWriter1.StartSegment('UNS');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: CNT' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CNT');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('4');
  ibeEDIFACTWriter1.WriteComponentString('7');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('9');
  ibeEDIFACTWriter1.WriteComponentString('67627.50');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: MOALoop4' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('MOALoop4/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('79');
  ibeEDIFACTWriter1.WriteComponentString('57554.00');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: TAXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: TAX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/TAX');
  ibeEDIFACTWriter1.WriteElementString('7');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VAT');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('17.500');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('125');
  ibeEDIFACTWriter1.WriteComponentString('57555.36');
  ibeEDIFACTWriter1.WriteComponentString('EUR');
  ibeEDIFACTWriter1.WriteComponentString('3');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('124');
  ibeEDIFACTWriter1.WriteComponentString('10072.14');
  ibeEDIFACTWriter1.WriteComponentString('EUR');
  ibeEDIFACTWriter1.WriteComponentString('3');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: ALCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: ALC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('ALCLoop1/ALC');
  ibeEDIFACTWriter1.WriteElementString('C');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('1.36');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FC');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: MOA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('TAXLoop1/MOA');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('8');
  ibeEDIFACTWriter1.WriteComponentString('1.36');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  ibeEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  ibeEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_ORDERS();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  ibeEDIFACTWriter1.StartInterchangeHeader('D97A');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('UNOB');
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ACME');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('160707');
  ibeEDIFACTWriter1.WriteComponentString('1547');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('000000003');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('1234');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  ibeEDIFACTWriter1.StartTransactionHeader('ORDERS');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ORDERS');
  ibeEDIFACTWriter1.WriteComponentString('D');
  ibeEDIFACTWriter1.WriteComponentString('97A');
  ibeEDIFACTWriter1.WriteComponentString('UN');
  ibeEDIFACTWriter1.WriteComponentString('ED17A1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('BGM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('105');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('K12345');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('9');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('137');
  ibeEDIFACTWriter1.WriteComponentString('19980626');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: FTX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('FTX');
  ibeEDIFACTWriter1.WriteElementString('GEN');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('FREE TEXT');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('PS');
  ibeEDIFACTWriter1.WriteComponentString('10501');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.WriteComponentString('NO');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('BY');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop2' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/RFFLoop2/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('VA');
  ibeEDIFACTWriter1.WriteComponentString('GB107328000');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('SE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('GBP');
  ibeEDIFACTWriter1.WriteComponentString('9');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('001');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0000057G3454');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INF');
  ibeEDIFACTWriter1.WriteComponentString('27.54');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('9829');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('JB');
  ibeEDIFACTWriter1.WriteComponentString('JOB NO');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('19980717');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('002');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0000057G3454');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('4000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INF');
  ibeEDIFACTWriter1.WriteComponentString('27.54');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('9830');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('JB');
  ibeEDIFACTWriter1.WriteComponentString('JOB NO');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('4000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('19980724');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('003');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('0000057G3454');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: IMD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/IMD');
  ibeEDIFACTWriter1.WriteElementString('F');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('DESCRIPTION');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('3000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('INF');
  ibeEDIFACTWriter1.WriteComponentString('27.54');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('9831');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('JB');
  ibeEDIFACTWriter1.WriteComponentString('JOB NO');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('21');
  ibeEDIFACTWriter1.WriteComponentString('3000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('19980731');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  ibeEDIFACTWriter1.StartSegment('UNS');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  ibeEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  ibeEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.writeFile_EDIFACT_ORDRSP();
begin
  txtLog.Text := txtLog.Text + 'StartInterchangeHeader' + #13#10;
  ibeEDIFACTWriter1.StartInterchangeHeader('D97A');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('UNOB');
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('WAYNE_TECH');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ACME');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('160707');
  ibeEDIFACTWriter1.WriteComponentString('1547');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('000000004');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('1234');
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.SkipElement();
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartTransactionHeader' + #13#10;
  ibeEDIFACTWriter1.StartTransactionHeader('ORDRSP');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ORDRSP');
  ibeEDIFACTWriter1.WriteComponentString('D');
  ibeEDIFACTWriter1.WriteComponentString('97A');
  ibeEDIFACTWriter1.WriteComponentString('UN');
  ibeEDIFACTWriter1.WriteComponentString('EDOR04');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: BGM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('BGM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('231');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('1109706372/3');
  ibeEDIFACTWriter1.EndElement();
  ibeEDIFACTWriter1.WriteElementString('9');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('137');
  ibeEDIFACTWriter1.WriteComponentString('20150708');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('RFFLoop1/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('ON');
  ibeEDIFACTWriter1.WriteComponentString('INCG14040002');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('SE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: NADLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: NAD' + #13#10;
  ibeEDIFACTWriter1.StartSegment('NADLoop1/NAD');
  ibeEDIFACTWriter1.WriteElementString('BY');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: CUXLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: CUX' + #13#10;
  ibeEDIFACTWriter1.StartSegment('CUXLoop1/CUX');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('USD');
  ibeEDIFACTWriter1.WriteComponentString('4');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('800');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.9600');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('800');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20140401');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150729');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('2');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.9600');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20141020');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150729');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('3');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRFS4115PBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.9600');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('3');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20141120');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150809');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('4');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('4000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.1000');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('4');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('4000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20140605');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150810');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('5');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('12000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.1000');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('5');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('12000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20140705');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150801');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: LINLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: LIN' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/LIN');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.WriteElementString('6');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('VP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('91');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: PIA' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PIA');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('IRLR8259TRPBF');
  ibeEDIFACTWriter1.WriteComponentString('BP');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('92');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('12000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: PRILoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: PRI' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/PRILoop1/PRI');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('AAA');
  ibeEDIFACTWriter1.WriteComponentString('0.1000');
  ibeEDIFACTWriter1.WriteComponentString('CT');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('1');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: RFFLoop3' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: RFF' + #13#10;
  ibeEDIFACTWriter1.StartSegment('LINLoop1/RFFLoop3/RFF');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('LI');
  ibeEDIFACTWriter1.SkipComponent();
  ibeEDIFACTWriter1.WriteComponentString('6');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('10000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20140805');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150805');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: SCCLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: SCC' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/SCC');
  ibeEDIFACTWriter1.WriteElementString('1');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'StartLoop: QTYLoop1' + #13#10;
  txtLog.Text := txtLog.Text + 'Segment: QTY' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/QTY');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('113');
  ibeEDIFACTWriter1.WriteComponentString('2000');
  ibeEDIFACTWriter1.WriteComponentString('PCE');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('2');
  ibeEDIFACTWriter1.WriteComponentString('20140805');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: DTM' + #13#10;
  ibeEDIFACTWriter1.StartSegment('SCCLoop1/QTYLoop1/DTM');
  ibeEDIFACTWriter1.StartElement();
  ibeEDIFACTWriter1.WriteComponentString('67');
  ibeEDIFACTWriter1.WriteComponentString('20150815');
  ibeEDIFACTWriter1.WriteComponentString('102');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'Segment: UNS' + #13#10;
  ibeEDIFACTWriter1.StartSegment('UNS');
  ibeEDIFACTWriter1.WriteElementString('S');
  ibeEDIFACTWriter1.EndElement();

  txtLog.Text := txtLog.Text + 'EndTransaction' + #13#10;
  ibeEDIFACTWriter1.CreateTransactionFooter();
  txtLog.Text := txtLog.Text + 'EndInterchange' + #13#10;
  ibeEDIFACTWriter1.CreateInterchangeFooter();
end;

procedure TFormEdifactwriter.btnWriteEDIClick(Sender: TObject);
begin
  try

    ibeEDIFACTWriter1.Reset();
    txtLog.Text := '';
    txtFile.Text := '';

    ibeEDIFACTWriter1.LoadSchema('RSSBus_D97A_' + cbFileType.Text +
    '.json');

    ibeEDIFACTWriter1.Suffix := TibeEDIFACTWriterSuffixes(3); // suffixCRLF

    ibeEDIFACTWriter1.Config('Encoding=iso-8859-1');
    writeFile();

  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
end;


end.


