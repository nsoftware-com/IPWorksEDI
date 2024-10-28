<?php
/*
 * IPWorks EDI 2024 PHP Edition - Sample Project
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
 */
require_once('../include/ipworksedi_as2sender.php');
require_once('../include/ipworksedi_certmgr.php');
require_once('../include/ipworksedi_const.php');
?>
<?php
class MyAS2Sender extends IPWorksEDI_AS2Sender{
  function FireSSLServerAuthentication($param) {
    $param['accept'] = true;
    return $param;
  }
}

try {

  if ($argc < 4) {
    echo "Usage: php as2client.php -f AS2FromId -t AS2ToId -u ReceiverURL -d EDIData -s SignatureCert -p SigningPass -r RecipientCert -m MDNTo\n\n";
    echo "  -f    The AS2 From ID to use.\n";
    echo "  -t    The AS2 To ID to use.\n";
    echo "  -u    The URL of the server that receives the data.\n";
    echo "  -d    The EDI data to send.\n";
    echo "  -s    The sender's private key to use for signing. [Optional] (specify if you want to sign the outgoing data)\n";
    echo "  -p    The password for the sender's private key. [Optional] (password for the provided as2sender.pfx file is \"test\")\n";
    echo "  -r    The recipient's certificate for encryption. [Optional] (specify if you want to encrypt the outgoing data)\n";
    echo "  -m    Where to deliver the MDN (default: as2@nsoftware.com) [Optional] (specify if you want to receive an MDN)\n";
    echo "\nExample: php as2client.php -f \"AS2 Test Sending Organization\" -t \"AS2 Test Receiving Organization\" -u \"http://localhost:1339/as2server.aspx\" "
        . " -d \"Sample EDI Data\" -m \"as2@nsoftware.com\" -s \"as2sender.pfx\" -p \"test\" -r \"as2receiver.cer\"\n";
    return;
  }

  $as2client = new MyAS2Sender();
  $signingCert = "";
  $signingCertPass = "";
  $mdnRequested = false;

  for ($i = 0; $i < $argc; $i++)
  {
    if (str_starts_with($argv[$i],"-")) {
      if ($argv[$i] == "-f") { 
        $as2client->setAS2From($argv[$i + 1]);
      }
      if ($argv[$i] == "-t") { 
        $as2client->setAS2To($argv[$i + 1]);
      }
      if ($argv[$i] == "-u") { 
        // Set your partner's URL (HTTP or HTTPS) and the data to be sent. Note that if you are posting
        // to an HTTPS URL, you will likely need to set SSLAcceptServerCert.
        $as2client->setURL($argv[$i + 1]);
      }
      if ($argv[$i] == "-d") {
        $as2client->setEDIType("application/edi-x12");
        $as2client->setEDIData($argv[$i + 1]);
      }
      if ($argv[$i] == "-s") { $signingCert = $argv[$i + 1]; }
      if ($argv[$i] == "-p") { $signingCertPass = $argv[$i + 1]; }
      if ($argv[$i] == "-r") {
        // Similarly, setting the recipient's certificate will instruct the class to encrypt the message.
        // If you want to set a certificate, but don't want to encrypt the message, you can set
        // EncryptionAlgorithm to an empty string.
        $as2client->setRecipientCertCount(1);
        $as2client->setRecipientCertStoreType(0, 99); // auto
        $as2client->setRecipientCertStore(0, $argv[$i + 1]);
        $as2client->setRecipientCertSubject(0, "*");
      }
      if ($argv[$i] == "-m") {
        $mdnRequested = true;
        // The actual value you feed to MDNTo is irrelevant,
        // Most servers just check to see if something is specified at all
        $as2client->setMDNTo($argv[$i + 1]);

        // For the purposes of this demo, we only support synchronous MDNs so we leave this blank
        $as2client->setMDNOptions("");

        // By default, the class will request that the receipt be delivered synchronously over the same
        // HTTP connection. If you prefer to receive your receipt asynchronously, you should set
        // MDNDeliveryOption, and provide additional processing for inbound asynchronous receipts.
        // as2->setMDNDeliveryOption("https://localhost:59144/");
       }
    }
  }

  if ($signingCert != "") {
    // Note that setting a signing certificate will instruct the class to sign the message.
    // Leave these properties blank to send an unsigned message.
    $as2client->setSigningCertStoreType(99);
    $as2client->setSigningCertStore($signingCert);
    $as2client->setSigningCertStorePassword($signingCertPass);
    $as2client->setSigningCertSubject("*");
  }

  // Some servers do not support chunked encoding. The following setting can be used to disable chunked encoding.
  // $as2client->doConfig("UseChunkedEncoding=false");

  // If you set a log directory, the component will produce detailed log files
  $as2client->setLogDirectory("logs");

  echo "Sending data to " . $as2client->getURL() . "...\n";

  try {
    // If the call to post() returns without throwing an exception, then the class was able to post
    // the data and verify the response. In particular, if you requested a synchronous MDN,
    // it will automatically be validated, and an exception will be thrown if there are any problems.

    // If you requested an asynchronous MDN, you will need to save the values of MessageId,
    // OriginalContentMIC, and MDNOptions, so they can be looked up based on the MessageId.
    // Then, when you process the asynchronous MDN, you will need to load these values into
    // the class to verify the MDN.
    $as2client->doPost();

    echo "Success!\n";

    if ($mdnRequested) {
      echo "MDN Verified\n";
      echo "======================================== MDN Headers ========================================\n";
      echo $as2client->getMDNReceiptHeaders() . "\n";
      echo "=============================================================================================\n";
      echo "============================= MDN Message (Human-Readable Part) =============================\n";
      echo $as2client->getMDNReceiptMessage() . "\n";
      echo "=============================================================================================\n";
      echo "====================================== Raw MDN Content ======================================\n";
      echo $as2client->getMDNReceiptContent() . "\n";
      echo "=============================================================================================\n";
    } else {
      echo "No MDN Requested\n";
    }
  } catch (Exception $ex) {
    echo "Sending failed.\n";
    echo "Reason: " . $ex->getMessage() . "\n";
  }
} catch (Exception $ex) {
  echo "Error: " . $ex->getMessage() . "\n";
}
?>
