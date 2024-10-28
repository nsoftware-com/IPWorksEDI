/*
 * IPWorks EDI 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksedi = require("@nsoftware/ipworksedi");

if(!ipworksedi) {
  console.error("Cannot find ipworksedi.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  let SIG_ALGS = ["SHA1", "MD5", "SHA-256", "SHA-384", "SHA-512", "SHA-224"];
  let ENC_ALGS = ["3DES", "DES", "AESCBC128", "AESCBC192", "AESCBC256"];
  let as2 = new ipworksedi.as2sender();
  
  as2.on("SSLServerAuthentication", function(e) {
    if (e.Accept) return;
    log("Server provided the following certificate:");
    log("Issuer: " + e.CertIssuer);
    log("Subject: " + e.CertSubject);
    log("The following problems have been determined for this certificate: " + e.Status);
    if (yesno("Would you like to continue anyway?", false)) e.Accept = true;
  });
  
  // Logs will be written to the following directory (for a description of what the macros do, see the documentation).
  as2.setLogDirectory("./AS2 Logs/To %as2to%/%date%/%messageid%");
  as2.config("LogOptions=All");

  // Some servers does not support chunked encoding (e.g. ASP.NET Development Server).
  // To work with these servers uncomment this line.
  //as2.config("UseChunkedEncoding=false");
  
  as2.setAS2From(getarg("from", "mycompanyAS2") + "");
  as2.setAS2To(getarg("to", "mendelsontestAS2") + "");
  as2.setURL(getarg("url", "http://testas2.mendelson-e-c.com:8080/as2/HttpReceiver") + "");

  as2.setCompressionFormat(1); // 0 = None, 1 = ZLIB.

  // Specify the signing algorithm to use. Default to SHA-256.
  as2.setSignatureAlgorithm(getarg("signaturealg", "SHA-256") + "");
  // Specify the signing certificate information. The signing certificate is the private certificate of the sender.
  try {
    let signingcert = getarg("signingcert", "./testserver_sender.pfx");
    let signingpass = getarg("signingpass", "test");
    let signingsubject = getarg("signingsubject", "*");
    as2.setSigningCert(new ipworksedi.Certificate(2, signingcert, signingpass, signingsubject));
  } catch(err) {
    console.log("Error with signing certificate: " + err.message);
  }

  // Specify the encryption algorithm to use. Default to 3DES.
  as2.setEncryptionAlgorithm(getarg("encryptalg", "3DES") + "");
  // Specify the encryption certificate information. The encryption certificate is the public certificate
  // of the receiver.
  try {
    let recipientcert = getarg("recipientkey", "./testserver_receiver.cer");
    let recipientsubject = getarg("recipientsubject", "*");
    as2.getRecipientCerts().add(new ipworksedi.Certificate(6, recipientcert, "", recipientsubject)); // .cer public key file (PEM-encoded).
  } catch(err) {
    console.log("Error with recipient certificate: " + err.message);
  }

  // To request an MDN (Message Disposition Notification) based receipt, you should set the MDNTo property to 
  // specify where to deliver the MDN to. (Note that the actual value is  irrelevant, as most servers just check
  // to see whether anything was specified at all).
  let mdnTo = getarg("mdnto", "as2@nsoftware.com");
  let mdnRequested = mdnTo !== "none";
  if (mdnRequested) {
    as2.setMDNTo(mdnTo);
    // By default the component will request a SIGNED receipt, with a Received-Content-MIC value that establishes
    // digital non-repudiation. If you prefer to receive an unsigned receipt, set MDNOptions to an empty string.

    // Typically, receivers use the same certificate for both decrypting incoming messages and signing outgoing
    // MDNs, so the AS2Sender component will use the first certificate held by the RecipientCert* properties to
    // both encrypt outgoing data and to verify incoming MDNs.

    // However, some receivers use an additional certificate just for signing outgoing MDNs, in which case we'll
    // need to set the ReceiptSignerCert* properties to let the AS2Sender component know that that's the case.
    // (Alternatively, we might need to set the ReceiptSignerCert* properties simply because we did not wish to
    // encrypt the outgoing message, and therefore did not set the RecipientCert* properties.)

    // Mendelson requires encryption, and uses the same certificate for both decryption and MDN signing, so the
    // defaults here reflect that use-case, but be sure to check with your partner if you use this demo to test.
    /*
    if (separateSignerCert) {
      try {
        let receiptSignerCert = "./testserver_receiver.cer";
        let receiptSignerCertSubject = "*";
        as2.setReceiptSignerCert(new ipworksedi.Certificate(6, receiptSignerCert, "", receiptSignerCertSubject)); // .cer public key file (PEM-encoded).
      } catch(err) {
        console.log("Error with MDN signer certificate: " + err.message);
      }
    }
    */
  }

  // Specify the data to send. For simplicity's sake, this demo just takes data from the '-data' command line argument.
  // If you wanted to send a file instead, you could set the EDIFileName property.
  let ediData = new ipworksedi.EDIData();
  ediData.Data = getarg("data", "This is a test.") + "";
  as2.setEDIData(ediData);

  // Print a summary of what's been configured.
  log("Sending using the following options:");
  log("AS2From: " + as2.getAS2From());
  log("AS2To: " + as2.getAS2To());
  log("URL: " + as2.getURL());
  log("Signature Algorithm: " + as2.getSignatureAlgorithm());
  log("Signing Cert Store: " + as2.getSigningCert().getStoreType());
  log("Signing Cert Password: " + as2.getSigningCert().getStorePassword());
  log("Signing Cert Subject: " + as2.getSigningCert().getSubject());
  log("Encryption Algorithm: " + as2.getEncryptionAlgorithm());
  log("Encryption Cert Store: " + as2.getRecipientCerts().get(0).getStore());
  log("Encryption Cert Subject: " + as2.getRecipientCerts().get(0).getSubject());
  if (mdnRequested) {
    log("Deliver MDN To: " + as2.getMDNTo());
  }
  log("Data to send: " + as2.getEDIData());
  log();

  // Now, we send the data using the Post() method. If you requested a synchronous MDN, then it's automatically
  // validated (this demo doesn't support asynchronous MDNs). If any issues occur, the Post() method will throw
  // an error.
  log("Sending data...");
  try{
    as2.post();
    log("Success! " + (mdnRequested ? "MDN Verified" : "No MDN Requested"));
    if (mdnRequested) {
      let receipt = as2.getMDNReceipt();
      log("======================================== MDN Headers ========================================");
      log(receipt.getHeaders());
      log("=============================================================================================");
      log("============================= MDN Message (Human-Readable Part) =============================");
      log(receipt.getMessage());
      log("=============================================================================================");
      log("============================ MDN Message (Machine-Readable Part) ============================");
      log(receipt.getMDN());
      log("=============================================================================================");
      log("====================================== Raw MDN Content ======================================");
      log(receipt.getContent());
      log("=============================================================================================");
    }
  }
  catch(err) {
    log("Failed.");
    log("Reason: " + err.message);
  }
  process.exit();
}

function getarg(arg, defaultVal) {
  let argv = process.argv;
  let index = argv.findIndex((current) => "-" + arg == current);
  if (index == -1) value = defaultVal; // Argument not found, use default value
  else if (index == argv.length - 1) value = null; // Argument at end of argv (No value)
  else if (argv[index + 1].startsWith("-")) value = null; // Argument followed by another argument (No value)
  else value = argv[index + 1]; // Use value of argument

  if (value == null) {
    showusage();
    process.exit(1);
   } // If the argument was present but the value was missing, show usage and exit.
  return value + "";
}

function log(s) {
  s = typeof s !== "undefined" ? s : "";
  console.log(s);
}

function showusage() {
  log("Usage: node as2client.js [options]");
  log("Options:");
  log("-from             the sender's AS2 identifier (default: mycompanyAS2)");
  log("-to               the receiver's AS2 identifier (default: mendelsontestAS2)");
  log("-url              the receiver's url")
  log("                    (default: http://testas2.mendelson-e-c.com:8080/as2/HttpReceiver)");
  log("-signaturealg     the signing algorithm to use (default: SHA-256)");
  log("                    (values: SHA1, MD5, SHA-256, SHA-384, SHA-512, SHA-224)");
  log("-signingcert      the sender's private key (a .pfx/.p12 file) to use for signing");
  log("                    (default: ./testserver_sender.pfx)");
  log("-signingpass      the password for the sender's private key(default: test)");
  log("-signingsubject   the subject for the sender's private key(default: *)");
  log("-encryptalg       the encryption algorithm to use (default: 3DES)");
  log("                    (values: 3DES, DES, AESCBC128, AESCBC192, AESCBC256");
  log("-recipientkey     the receiver's public key (a .cer file) to use for encryption");
  log("                    (default: ./testserver_receiver.cer)");
  log("-recipientsubject the subject for the receiver's public key (default: *)");
  log("-mdnto            where to deliver the MDN(default: as2@nsoftware.com)");
  log("-data             the data to send (default: \"This is a test.\")");
}

async function hasflag(flag) {
  let index = process.argv.findIndex((current) => "-" + flag == current);
  return index >= 0;
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
