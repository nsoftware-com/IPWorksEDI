<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks EDI 2022 Demos - AS2 Client</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks EDI 2022 Demos - AS2 Client">
</head>

<body>

<div id="content">
<h1>IPWorks EDI - Demo Pages</h1>
<h2>AS2 Client</h2>
<p>A simple but full-featured AS2 client.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworksedi_as2sender.php');
require_once('../include/ipworksedi_certmgr.php');
require_once('../include/ipworksedi_const.php');

?>

<?php $method = $_SERVER["REQUEST_METHOD"]; ?>
<style>
table { width: 100% !important; }
td { white-space: nowrap; }
td input { width: 100%; }
td:last-child { width: 100%; }
</style>
<div width="90%">
  <form method=POST>
    <h2>Request Options</h2>
    <h3>General</h3>
    <table>
      <tr><td>AS2 From:</td><td><input type=text name=as2From value="<?php echo isset($_POST["as2From"]) ? $_POST["as2From"] : "AS2 Test Sending Organization" ?>"></td></tr>
      <tr><td>AS2 To:</td><td><input type=text name=as2To value="<?php echo isset($_POST["as2To"]) ? $_POST["as2To"] : "AS2 Test Receiving Organization" ?>"></td></tr>
      <tr><td>Receiver URL:</td><td><input type=text name=url value="<?php echo isset($_POST["url"]) ? $_POST["url"] : "http://localhost/ibephp16/as2server.php" ?>"></td></tr>
      <tr><td>MDN To*:</td><td><input type=text name=mdnTo value="<?php echo isset($_POST["mdnTo"]) ? $_POST["mdnTo"] : "as2@nsoftware.com" ?>"></td></tr>
    </table>
    <small>*Note that the value given here generally doesn't matter. AS2 receivers generally just check to see if <i>something</i> is specified when determining whether to send an MDN back or not.</small>
    <br/>
    <br/>
    <textarea name=ediData rows=10 style="width: 100%;">Paste EDI data here.</textarea>
    <br/>
    <br/>
    <div style="display: inline-block; margin: .25em 0;">
      <input type=checkbox name=sign <?php echo ($method != "POST" || isset($_POST["sign"])) ? "checked" : "" ?>>
      <label for=sign>Signed</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=checkbox name=encrypt <?php echo ($method != "POST" || isset($_POST["encrypt"])) ? "checked" : "" ?>>
      <label for=encrypt>Encrypted</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=checkbox name=compress <?php echo ($method != "POST" || isset($_POST["compress"])) ? "checked" : "" ?>>
      <label for=compress>Compressed</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=checkbox name=signMdn <?php echo ($method != "POST" || isset($_POST["signMdn"])) ? "checked" : "" ?>>
      <label for=signMdn>Request Signed MDN</lable>
    </div>
    <input type=submit value="Send" style="width: 10em; float: right;">
    <h3>Signing Options</h3>
    <b>Signature Algorithm</b>
    <br/>
    <div style="display: inline-block; margin: .25em 0;">
      <?php $defSigAlg = $method == "POST" ? $_POST["sigAlg"] : "sha-256" ?>
      <input type=radio id=sigAlg1 name=sigAlg value="sha1" <?php echo $defSigAlg == "sha1" ? "checked" : "" ?>>
      <label for=sigAlg1>SHA1</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigAlg2 name=sigAlg value="md5" <?php echo $defSigAlg == "md5" ? "checked" : "" ?>>
      <label for=sigAlg2>MD5</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigAlg3 name=sigAlg value="sha-256" <?php echo $defSigAlg == "sha-256" ? "checked" : "" ?>>
      <label for=sigAlg3>SHA-256</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigAlg4 name=sigAlg value="sha-384" <?php echo $defSigAlg == "sha-384" ? "checked" : "" ?>>
      <label for=sigAlg4>SHA-384</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigAlg5 name=sigAlg value="sha-512" <?php echo $defSigAlg == "sha-512" ? "checked" : "" ?>>
      <label for=sigAlg5>SHA-512</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=sigAlg6 name=sigAlg value="sha-224" <?php echo $defSigAlg == "sha-224" ? "checked" : "" ?>>
      <label for=sigAlg3>SHA-224</lable>
    </div>
    <br/>
    <b>Sender's Private Key (used for signing)</b>
    <table>
      <tr><td>.pfx/.p12 Certificate File:</td><td><input type=text name=sCertFile value="<?php echo isset($_POST["sCertFile"]) ? $_POST["sCertFile"] : "./as2sender.pfx" ?>"></td></tr>
      <tr><td>Password:</td><td><input type=text name=sCertPass value="<?php echo isset($_POST["sCertPass"]) ? $_POST["sCertPass"] : "test" ?>"></td></tr>
      <tr><td>Subject:</td><td><input type=text name=sCertSub value="<?php echo isset($_POST["sCertSub"]) ? $_POST["sCertSub"] : "*" ?>"></td></tr>
    </table>
    <h3>Encryption Options</h3>
    <b>Encryption Algorithm</b>
    <br/>
    <div style="display: inline-block; margin: .25em 0;">
      <?php $defEncAlg = $method == "POST" ? $_POST["encAlg"] : "3des" ?>
      <input type=radio id=encAlg1 name=encAlg value="3des" <?php echo $defEncAlg == "3des" ? "checked" : "" ?>>
      <label for=encAlg1>3DES</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=encAlg2 name=encAlg value="des" <?php echo $defEncAlg == "des" ? "checked" : "" ?>>
      <label for=encAlg2>DES</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=encAlg3 name=encAlg value="aescbc128" <?php echo $defEncAlg == "aescbc128" ? "checked" : "" ?>>
      <label for=encAlg3>AESCBC128</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=encAlg4 name=encAlg value="aescbc192" <?php echo $defEncAlg == "aescbc192" ? "checked" : "" ?>>
      <label for=encAlg4>AESCBC192</lable>
      &nbsp;&nbsp;&nbsp;
      <input type=radio id=encAlg5 name=encAlg value="aescbc256" <?php echo $defEncAlg == "aescbc256" ? "checked" : "" ?>>
      <label for=encAlg5>AESCBC256</lable>
    </div>
    <br/>
    <b>Receiver's Public Key (used for encryption and verifying signed MDNs)</b>
    <table>
      <tr><td>.cer Certificate File:</td><td><input type=text name=eCertFile value="<?php echo isset($_POST["eCertFile"]) ? $_POST["eCertFile"] : "./as2receiver.cer" ?>"></td></tr>
      <tr><td>Subject:</td><td><input type=text name=eCertSub value="<?php echo isset($_POST["eCertSub"]) ? $_POST["eCertSub"] : "*" ?>"></td></tr>
    </table>
  </form>
<?php
if ($method == "POST") {
  // Create an AS2Sender instance and set up logging settings.
  $as2 = new IPWorksEDI_AS2Sender();
  // (This is commented out by default to prevent any errors which may occur if the user PHP runs as doesn't
  // have access to the default log directory, which is alongside this demo file.)
  //$as2->setLogDirectory(__DIR__."/AS2 Logs/%date%/To %as2to%/%messageid%");
  $as2->doConfig("LogOptions=All");

  // The ASP.NET Development Server does not support chunked encoding, so this demo disables chunked encoding.
  // It can be enabled if you are not posting to the ASP.NET Development Server.
  $as2->doConfig("UseChunkedEncoding=False");

  // Populate the component's properties using the values from the POST.
  $as2->setAS2From($_POST["as2From"]);
  $as2->setAS2To($_POST["as2To"]);
  $as2->setURL($_POST["url"]);
  $as2->setMDNTo($_POST["mdnTo"]);
  if (!isset($_POST["signMdn"])) $as2->setMDNOptions("");
  $as2->setCompressionFormat(isset($_POST["compress"]) ? 1 : 0);
  $as2->setEDIData($_POST["ediData"]);
  if (isset($_POST["sign"])) {
    $as2->setSignatureAlgorithm($_POST["sigAlg"]);
    $as2->setSigningCertStoreType(2);
    $as2->setSigningCertStore($_POST["sCertFile"]);
    $as2->setSigningCertStorePassword($_POST["sCertPass"]);
    $as2->setSigningCertSubject($_POST["sCertSub"]);
  }
  if (isset($_POST["encrypt"])) {
    $as2->setEncryptionAlgorithm($_POST["encAlg"]);
    $as2->setRecipientCertCount(1);
    $as2->setRecipientCertStoreType(0, 6);
    $as2->setRecipientCertStore(0, $_POST["eCertFile"]);
    $as2->setRecipientCertSubject(0, $_POST["eCertSub"]);
  } elseif (isset($_POST["signMdn"])) {
    // We still need the receiver's public certificate if we want a signed MDN, even if we don't want to encrypt.
    // We'll just set it to the ReceiptSignerCert* properties rather than the RecipientCert* properties.
    $as2->setReceiptSignerCertStoreType(6);
    $as2->setReceiptSignerCertStore($_POST["eCertFile"]);
    $as2->setReceiptSignerCertSubject($_POST["eCertSub"]);
  }

  echo "    <hr/><hr/><hr/>";
  try {
    // Now, send the AS2 message.
    $as2->doPost();
    // If the call to post returns without throwing an exception, then the component was able to post the data and
    // verify the response. In particular, if you requested a synchronous MDN, it will automatically be validated, 
    // and an exception will be thrown if there are any problems.
    echo "    <h2>Transmission Successful! (Details Below)</h2>";
  } catch (Exception $ex) {
    echo "    <h2>Transmission Failed. (Details Below)</h2>";
    echo "    <p>Error: ".$ex->getMessage()."</p>";
  }
?>
  <h3>Headers</h3>
  <pre><?php echo $as2->getMDNReceiptHeaders() ?></pre>
  <h3>Human-Readable Message</h3>
  <pre><?php echo $as2->getMDNReceiptMessage() ?></pre>
  <h3>MDN Receipt</h3>
  <pre><?php echo $as2->getMDNReceiptMDN() ?></pre>
  <h3>Full Raw Response</h3>
  <pre><?php echo $as2->getMDNReceiptContent() ?></pre>
<?php
}
?>
</div>
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
Copyright (c) 2024 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks EDI 2022 - Copyright (c) 2024 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-BEPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
