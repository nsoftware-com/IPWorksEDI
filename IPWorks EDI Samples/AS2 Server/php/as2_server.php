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
require_once('../include/ipworksedi_as2receiver.php');
require_once('../include/ipworksedi_const.php');
?>
<?php
if (php_sapi_name() == "cli") {
  echo "This AS2Receiver demo is designed to be used in a web interface. Host this file on your PHP web server and post to it using the as2_client.php console demo.\n\n";
  echo "For quick testing, the PHP built-in web server is useful, which can be started using the following command from the demos directory:\n\n";
  echo "php -S 0.0.0.0:8000\n\n";
  echo "Once this is running, post to \"http://localhost:8000/as2_server.php\" from as2_client.php.\n";
  return;
}

$sendBuffer = TRUE; ob_start();

if (!function_exists('getallheaders'))
{
  function getallheaders()
  {
    $headers = array();
    foreach ($_SERVER as $name => $value)
    {
      if (substr($name, 0, 5) == 'HTTP_')
      {
        $headers[str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))))] = $value;
      }
    }
    return $headers;
  }
} 

if ($_SERVER["REQUEST_METHOD"] == "POST") {
  // Since this demo needs to be able to send responses back, we'll make sure that the boilerplate HTML isn't sent.
  $sendBuffer = FALSE;

  // Capture the headers and the body of the request to use later.
  $headers = getallheaders();
  $body = file_get_contents("php://input");

  // Create an AS2Receiver instance and set up logging settings.
  $as2 = new IPWorksEDI_AS2Receiver();
  // (This is commented out by default to prevent any errors which may occur if the user PHP runs as doesn't
  // have access to the default log directory, which is alongside this demo file.)
  // $as2->setLogDirectory(__DIR__."/AS2 Logs/%date%/From %as2from%/%messageid%");
  $as2->doConfig("LogOptions=All");

  // Now assign the request headers and body to AS2Sender.
  $as2->setRequestHeaderCount(count($headers));
  $idx = 0;
  foreach ($headers as $field => $value) {
    $as2->setRequestHeaderField($idx, $field);
    $as2->setRequestHeaderValue($idx, $value);
    $idx++;
  }
  $as2->setRequest($body);

  $receiptMessage = "";
  try {
    // Now we'll call ReadRequest() to parse the headers of the request. This will populate a variety of properties
    // and configurations settings. Refer to the documentation for more details.
    $as2->doReadRequest();

    // In a real-world scenario, you could now do things like check AS2To to make sure this message is for you, check
    // AS2From to see if the sender is valid and determine what certificates to load, etc. Since this demo assumes
    // you're using the test certificates provided, we'll load those now before attempting to parse the request.

    // The Cert* properties are used to specify the certificate for decryption (and signing the MDN, if necessary),
    // and should be set to the receiver's (that is, _your_) private key (a .pfx or .p12 file).
    // The SignerCert* properties are used to specify the certificate used to verify the sender's signature, and as
    // such should be set to the sender's public key (a .cer file).
    $as2->setCertStoreType(2); // a .pfx or .p12 file.
    $as2->setCertStore("./as2receiver.pfx");
    $as2->setCertStorePassword("test");
    $as2->setCertSubject("*");
    $as2->setSignerCertStoreType(6); // a .cer file (PEM-encoded).
    $as2->setSignerCertStore("./as2sender.cer");
    $as2->setSignerCertSubject("*");

    // Now that we have the correct certificates set up, we can use the ParseRequest() method to parse the request body.
    // (Note that if we hadn't already called ReadRequest(), it would also parse the request headers too.)

    // If ParseRequest() was successful in reading the request body (decompressing, decrypting, and verifying it along
    // the way, as necessary), then the Request property will have been cleared, and the EDIData property will now hold
    // the parsed request body's content. In addition, a few configs will have been populated with more information
    // about the request itself (such as the encryption type used, and other such information).

    // If there was an issue, the Request and EDIData properties remain unchanged, the ParseRequest() method will have
    // thrown an exception, and you can query ScanResult to determine an AS2-specific reason for why the request
    // parsing failed. You can still generate an MDN in this case, it will just contain details about the error.

    // So, now we'll parse the request.
    $as2->doParseRequest();
  } catch (Exception $ex) {
    // If an error occurs either when reading the request headers or when parsing the whole request, make sure that
    // we get things ready to create the MDN with the error information.
    $as2->doConfig("ProcessingError=True");
    $receiptMessage = $ex->getMessage();
  } 

  try {
    // Now create the MDN receipt. If there was an error, the message will override the default message.
    $as2->doCreateMDNReceipt("", "", $receiptMessage);

    // Finally, send back the MDN, headers first, then body.
    $mdnHeaders = explode("\r\n", $as2->getMDNReceiptHeaders());
    foreach ($mdnHeaders as $header) {
      if (strlen($header) > 0) header($header);
    }
    ob_end_clean(); // Clear all content output.
    echo $as2->getMDNReceiptContent();
  } catch (Exception $ex) {
    ob_end_clean();
    http_response_code(400);
    echo "Error: ".$ex->getMessage();
  } 
    
  flush(); // Ensure headers and body are flushed.

  // Resume capturing the boilerplate output, we'll discard it since allowing it to be included in our response to
  // the sender will cause problems.
  ob_start();
} else {
?>

<html>
<head>
<title>IPWorks EDI - AS2Receiver Demo</title>
</head>
<body>
<div>
<h2>AS2 Server</h2>
<p>A simple example of an AS2 server.</p>
<hr/>
<p>This demo shows how to use the <b>AS2Receiver</b> component to process and respond to incoming AS2 messages.</p>
<p>To try this demo, please post to this page using the IPWorks EDI PHP Edition <b>as2client</b> demo or other EDI client software.</p>
</div>
</body>
</html>

<?php
} 
if ($sendBuffer) ob_end_flush(); else ob_end_clean();
?>