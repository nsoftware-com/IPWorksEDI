<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks EDI 2022 Demos - X12 Writer</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks EDI 2022 Demos - X12 Writer">
</head>

<body>

<div id="content">
<h1>IPWorks EDI - Demo Pages</h1>
<h2>X12 Writer</h2>
<p>This demo shows how to use X12Writer to create X12 documents.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworksedi_x12writer.php');
require_once('../include/ipworksedi_const.php');

?>

<?php
//to print generated data to file, before calling the appropriate writeFile method, set
//x12writer1.setOutputFile("filename.txt");

$x12writer1 = new IPWorksEDI_X12writer();

function writeFile_X12_810($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000006");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("IN");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("810");
  $x12writer1->doWriteElementString("810");
  $x12writer1->doWriteElementString("0001");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BIG");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doWriteElementString("3003014445");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("0476553272");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("DR");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CUR");
  $x12writer1->doWriteElementString("SE");
  $x12writer1->doWriteElementString("USD");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("8M");
  $x12writer1->doWriteElementString("0056");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doWriteElementString("Company");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("544380");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("CA");
  $x12writer1->doWriteElementString("Postal Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0607047800010");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("200131");
  $x12writer1->doWriteElementString("Country");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("RE");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("5095956");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("IL");
  $x12writer1->doWriteElementString("Postal Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/IT1");
  $x12writer1->doWriteElementString("20");
  $x12writer1->doWriteElementString("2500");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("36.96");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0594");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/REF_3");
  $x12writer1->doWriteElementString("KK");
  $x12writer1->doWriteElementString("0099778154");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("IT1Loop1/REF_3");
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("0476553272");
  $x12writer1->doWriteElementString("20");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TDS");
  $x12writer1->doWriteElementString("9240000");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_850($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000007");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("850");
  $x12writer1->doWriteElementString("850");
  $x12writer1->doWriteElementString("0001");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BEG");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("DS");
  $x12writer1->doWriteElementString("0476696888");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("SB");
  $x12writer1->doWriteElementString("ZZ11");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("6P");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("8M");
  $x12writer1->doWriteElementString("0056");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("CR");
  $x12writer1->doWriteElementString("1070335099");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("REF");
  $x12writer1->doWriteElementString("CO");
  $x12writer1->doWriteElementString("7109790082");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PER");
  $x12writer1->doWriteElementString("CN");
  $x12writer1->doWriteElementString("name");
  $x12writer1->doWriteElementString("TE");
  $x12writer1->doWriteElementString("Number");

  $x12writer1->doStartSegment("CSH");
  $x12writer1->doWriteElementString("BK");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SACLoop1/SAC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doWriteElementString("ZZZZ");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("06");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TD5");
  $x12writer1->doWriteElementString("Z");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("Code");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N9Loop1/N9");
  $x12writer1->doWriteElementString("PD");
  $x12writer1->doWriteElementString("ZCOF");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N9Loop1/MSG");
  $x12writer1->doWriteElementString("Thanks!");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("5601");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("EN");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("OEM NAME");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0000505462");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N2");
  $x12writer1->doWriteElementString("additional name");
  $x12writer1->doWriteElementString(""); // not skipped because last element
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("201613");
  $x12writer1->doWriteElementString("CN");
  $x12writer1->doWriteElementString("SP");
  $x12writer1->doWriteElementString("020");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PO1");
  $x12writer1->doWriteElementString("00010");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("495");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("337S3744");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PIDLoop1/PID_2");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("Thanks!");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("EN");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF_7");
  $x12writer1->doWriteElementString("CO");
  $x12writer1->doWriteElementString("7109790082");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF_7");
  $x12writer1->doWriteElementString("LI");
  $x12writer1->doWriteElementString("000010");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/SCHLoop1/SCH");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("002");
  $x12writer1->doWriteElementString("20180708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTTLoop1/CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("500000");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_855($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000008");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("PR");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("8");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("855");
  $x12writer1->doWriteElementString("855");
  $x12writer1->doWriteElementString("0013");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BAK");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("AT");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("4900043704");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/PO1");
  $x12writer1->doWriteElementString("000010");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("14.00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0548");
  $x12writer1->doWriteElementString("VP");
  $x12writer1->doWriteElementString("Product");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF");
  $x12writer1->doWriteElementString("PO");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/REF");
  $x12writer1->doWriteElementString("VN");
  $x12writer1->doWriteElementString("0025009879");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PO1Loop1/ACKLoop1/ACK");
  $x12writer1->doWriteElementString("IA");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doWriteElementString("067");
  $x12writer1->doWriteElementString("20150709");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTTLoop1/CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_X12_856($x12writer1) {
  $x12writer1->doStartInterchangeHeader("004010");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("00");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("ZZ");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("U");
  $x12writer1->doWriteElementString("00401");
  $x12writer1->doWriteElementString("000000009");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString(">");
  $x12writer1->doEndElement();

  $x12writer1->doStartFunctionalGroupHeader();
  $x12writer1->doWriteElementString("SH");
  $x12writer1->doWriteElementString("ACME");
  $x12writer1->doWriteElementString("WAYNE_TECH");
  $x12writer1->doWriteElementString("20160707");
  $x12writer1->doWriteElementString("1544");
  $x12writer1->doWriteElementString("9");
  $x12writer1->doWriteElementString("T");
  $x12writer1->doWriteElementString("004010");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("856");
  $x12writer1->doWriteElementString("856");
  $x12writer1->doWriteElementString("0029");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BSN");
  $x12writer1->doWriteElementString("00");
  $x12writer1->doWriteElementString("0403734501");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doWriteElementString("162859");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doWriteElementString("011");
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("S");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/PRF");
  $x12writer1->doWriteElementString("0476553696");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("20150708");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/TD1");
  $x12writer1->doWriteElementString("CNT90");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/TD5");
  $x12writer1->doWriteElementString("O");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("FEDX");
  $x12writer1->doWriteElementString("A");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/REF");
  $x12writer1->doWriteElementString("BM");
  $x12writer1->doWriteElementString("EDITEST403734501");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/REF");
  $x12writer1->doWriteElementString("CR");
  $x12writer1->doWriteElementString("4900043704");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("O");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N1");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doWriteElementString("Name");
  $x12writer1->doWriteElementString("92");
  $x12writer1->doWriteElementString("0042001808");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N1");
  $x12writer1->doWriteElementString("SF");
  $x12writer1->doWriteElementString("NameT");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N3");
  $x12writer1->doWriteElementString("Address");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/N1Loop1/N4");
  $x12writer1->doWriteElementString("City");
  $x12writer1->doWriteElementString("SG");
  $x12writer1->doWriteElementString("339942");
  $x12writer1->doWriteElementString("SG");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/HL");
  $x12writer1->doWriteElementString("3");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("I");
  $x12writer1->doWriteElementString("0");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/LIN");
  $x12writer1->doWriteElementString("10");
  $x12writer1->doWriteElementString("BP");
  $x12writer1->doWriteElementString("335S0548");
  $x12writer1->doWriteElementString("VP");
  $x12writer1->doWriteElementString("Product");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/SN1");
  $x12writer1->doWriteElementString("10");
  $x12writer1->doWriteElementString("1100");
  $x12writer1->doWriteElementString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("HLLoop1/MAN");
  $x12writer1->doWriteElementString("CP");
  $x12writer1->doWriteElementString("Marks");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CTT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateFunctionalGroupFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_EDIFACT_DESADV($x12writer1) {
  $x12writer1->doStartInterchangeHeader("D97A");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("UNOB");
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("WAYNE_TECH");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ACME");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("160707");
  $x12writer1->doWriteComponentString("1547");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("000000001");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("1234");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("DESADV");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("DESADV");
  $x12writer1->doWriteComponentString("D");
  $x12writer1->doWriteComponentString("97A");
  $x12writer1->doWriteComponentString("UN");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BGM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("351");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2014/10093");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("9");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("137");
  $x12writer1->doWriteComponentString("201404192036");
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("201404192036");
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MEA");
  $x12writer1->doWriteElementString("AAX");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("SQ");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("C62");
  $x12writer1->doWriteComponentString("17");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("ST");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0018");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("SU");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2019813");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TDTLoop1/TDT");
  $x12writer1->doWriteElementString("12");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("M");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("CARRIER");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("86");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("EQDLoop1/EQD");
  $x12writer1->doWriteElementString("TE");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("X");
  $x12writer1->doEndElement();
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/CPS");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/PACLoop1/PAC");
  $x12writer1->doWriteElementString("4");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("BOX-001");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/PACLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("52");
  $x12writer1->doWriteComponentString("50");
  $x12writer1->doWriteComponentString("C62");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/CPS");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/PACLoop1/PAC");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("BOX-002");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/PACLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("52");
  $x12writer1->doWriteComponentString("100");
  $x12writer1->doWriteComponentString("C62");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/LINLoop1/LIN");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("9001");
  $x12writer1->doWriteComponentString("IN");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CPSLoop1/LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("12");
  $x12writer1->doWriteComponentString("400");
  $x12writer1->doWriteComponentString("C62");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("RFFLoop1/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ON");
  $x12writer1->doWriteComponentString("N55109001");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_EDIFACT_INVOIC($x12writer1) {
  $x12writer1->doStartInterchangeHeader("D97A");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("UNOB");
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("WAYNE_TECH");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ACME");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("160707");
  $x12writer1->doWriteComponentString("1547");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("000000002");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("1234");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("INVOIC");
  $x12writer1->doWriteElementString("509010117");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INVOIC");
  $x12writer1->doWriteComponentString("D");
  $x12writer1->doWriteComponentString("97A");
  $x12writer1->doWriteComponentString("UN");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BGM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("380");
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("TAX INVOICE");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0013550417");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("9");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("3");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("4");
  $x12writer1->doWriteComponentString("20061123");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("FTX");
  $x12writer1->doWriteElementString("AAI");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("10072.14");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CUXLoop1/CUX");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("EUR");
  $x12writer1->doWriteComponentString("4");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("0.67529");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PATLoop1/PAT");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PATLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("10");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("PATLoop1/PCD");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("0");
  $x12writer1->doWriteComponentString("13");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000030");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2265S13");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5029766832002");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("50.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("19150.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("383.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("19150.45");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.45");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.45");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000040");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2269F22");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5051254078241");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("20.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("21060.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("1053.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("21060.50");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.50");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.50");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000170");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2269F10");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5051254078326");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("10.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("6950.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("695.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("6950.16");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.16");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.16");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000190");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2269F26");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5051254051190");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("5.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("2375.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("475.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("2375.06");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.06");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.06");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000200");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2265S24");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5029766000685");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("3.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("957.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("319.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("957.02");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.02");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.02");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000210");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2263T95");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5029766699575");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("3.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("2085.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("695.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("2085.05");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.05");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.05");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("000250");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2269F15");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("5051254080091");
  $x12writer1->doWriteComponentString("UP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doWriteElementString("");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("47");
  $x12writer1->doWriteComponentString("3.000");
  $x12writer1->doWriteComponentString("EA");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("11");
  $x12writer1->doWriteComponentString("20070926");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("203");
  $x12writer1->doWriteComponentString("4977.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INV");
  $x12writer1->doWriteComponentString("1659.00");
  $x12writer1->doWriteComponentString("TU");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("4977.12");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0.12");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("0.12");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("UNS");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CNT");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("4");
  $x12writer1->doWriteComponentString("7");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("9");
  $x12writer1->doWriteComponentString("67627.50");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("MOALoop4/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("79");
  $x12writer1->doWriteComponentString("57554.00");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/TAX");
  $x12writer1->doWriteElementString("7");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VAT");
  $x12writer1->doEndElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("17.500");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("125");
  $x12writer1->doWriteComponentString("57555.36");
  $x12writer1->doWriteComponentString("EUR");
  $x12writer1->doWriteComponentString("3");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("124");
  $x12writer1->doWriteComponentString("10072.14");
  $x12writer1->doWriteComponentString("EUR");
  $x12writer1->doWriteComponentString("3");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("ALCLoop1/ALC");
  $x12writer1->doWriteElementString("C");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("1.36");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FC");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("TAXLoop1/MOA");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("8");
  $x12writer1->doWriteComponentString("1.36");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_EDIFACT_ORDERS($x12writer1) {
  $x12writer1->doStartInterchangeHeader("D97A");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("UNOB");
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("WAYNE_TECH");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ACME");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("160707");
  $x12writer1->doWriteComponentString("1547");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("000000003");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("1234");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("ORDERS");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ORDERS");
  $x12writer1->doWriteComponentString("D");
  $x12writer1->doWriteComponentString("97A");
  $x12writer1->doWriteComponentString("UN");
  $x12writer1->doWriteComponentString("ED17A1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BGM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("105");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("K12345");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("9");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("137");
  $x12writer1->doWriteComponentString("19980626");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("FTX");
  $x12writer1->doWriteElementString("GEN");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("FREE TEXT");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("RFFLoop1/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("PS");
  $x12writer1->doWriteComponentString("10501");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("RFFLoop1/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doWriteComponentString("NO");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/RFFLoop2/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("VA");
  $x12writer1->doWriteComponentString("GB107328000");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("SE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CUXLoop1/CUX");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("GBP");
  $x12writer1->doWriteComponentString("9");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("001");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0000057G3454");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("DESCRIPTION");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INF");
  $x12writer1->doWriteComponentString("27.54");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("9829");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("JB");
  $x12writer1->doWriteComponentString("JOB NO");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("19980717");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("002");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0000057G3454");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("DESCRIPTION");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("4000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INF");
  $x12writer1->doWriteComponentString("27.54");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("9830");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("JB");
  $x12writer1->doWriteComponentString("JOB NO");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("4000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("19980724");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("003");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("0000057G3454");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/IMD");
  $x12writer1->doWriteElementString("F");
  $x12writer1->doSkipElement();
  $x12writer1->doStartElement();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("DESCRIPTION");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("3000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("INF");
  $x12writer1->doWriteComponentString("27.54");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("9831");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("JB");
  $x12writer1->doWriteComponentString("JOB NO");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("21");
  $x12writer1->doWriteComponentString("3000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("19980731");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("UNS");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateInterchangeFooter();
}

function writeFile_EDIFACT_ORDRSP($x12writer1) {
  $x12writer1->doStartInterchangeHeader("D97A");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("UNOB");
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("WAYNE_TECH");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ACME");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("160707");
  $x12writer1->doWriteComponentString("1547");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("000000004");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("1234");
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doSkipElement();
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartTransactionHeader("ORDRSP");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ORDRSP");
  $x12writer1->doWriteComponentString("D");
  $x12writer1->doWriteComponentString("97A");
  $x12writer1->doWriteComponentString("UN");
  $x12writer1->doWriteComponentString("EDOR04");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("BGM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("231");
  $x12writer1->doEndElement();
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("1109706372/3");
  $x12writer1->doEndElement();
  $x12writer1->doWriteElementString("9");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("137");
  $x12writer1->doWriteComponentString("20150708");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("RFFLoop1/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("ON");
  $x12writer1->doWriteComponentString("INCG14040002");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("SE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("NADLoop1/NAD");
  $x12writer1->doWriteElementString("BY");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("CUXLoop1/CUX");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("USD");
  $x12writer1->doWriteComponentString("4");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("800");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.9600");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("800");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20140401");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150729");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("2");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.9600");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20141020");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150729");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("3");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRFS4115PBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.9600");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("3");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20141120");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150809");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("4");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("4000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.1000");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("4");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("4000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20140605");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150810");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("5");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("12000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.1000");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("5");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("12000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20140705");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150801");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/LIN");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doWriteElementString("6");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("VP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("91");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PIA");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("IRLR8259TRPBF");
  $x12writer1->doWriteComponentString("BP");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("92");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("12000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/PRILoop1/PRI");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("AAA");
  $x12writer1->doWriteComponentString("0.1000");
  $x12writer1->doWriteComponentString("CT");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("1");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("LINLoop1/RFFLoop3/RFF");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("LI");
  $x12writer1->doSkipComponent();
  $x12writer1->doWriteComponentString("6");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("10000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20140805");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150805");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/SCC");
  $x12writer1->doWriteElementString("1");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/QTY");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("113");
  $x12writer1->doWriteComponentString("2000");
  $x12writer1->doWriteComponentString("PCE");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("2");
  $x12writer1->doWriteComponentString("20140805");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("SCCLoop1/QTYLoop1/DTM");
  $x12writer1->doStartElement();
  $x12writer1->doWriteComponentString("67");
  $x12writer1->doWriteComponentString("20150815");
  $x12writer1->doWriteComponentString("102");
  $x12writer1->doEndElement();

  $x12writer1->doStartSegment("UNS");
  $x12writer1->doWriteElementString("S");
  $x12writer1->doEndElement();

  $x12writer1->doCreateTransactionFooter();

  $x12writer1->doCreateInterchangeFooter();
}

if ($_SERVER['REQUEST_METHOD'] == "POST") {
  try{
    $x12writer1->doConfig("Encoding=iso-8859-1");
    $x12writer1->setSuffix(3); // CRLF
    $x12writer1->doLoadSchema(getcwd() . "/RSSBus_00401_810.json");
    if($_POST["docType"] == "810"){
      writeFile_X12_810($x12writer1);
    }else if($_POST["docType"] == "850"){
      writeFile_X12_850($x12writer1);
    }else if($_POST["docType"] == "855"){
      writeFile_X12_855($x12writer1);
    }else if($_POST["docType"] == "856"){
      writeFile_X12_856($x12writer1);
    }
    echo '<p>' . $x12writer1->getOutputData() . '</p>';
  }catch(Exception $ex){
    echo '<p>There was an issue. [' . $x12writer1->lastErrorCode() . ']: ' . $x12writer1->lastError() . '</p>';
  }
}

?>

<form method="POST">
<select name="docType">
  <option value="810">810</option>
  <option value="850">850</option>
  <option value="855">855</option>
  <option value="856">856</option>
  <input type="submit" name="write" value="Write Document" />
</form>


<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks EDI objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-BEPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks EDI 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-BEPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
