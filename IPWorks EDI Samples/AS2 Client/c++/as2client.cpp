/*
 * IPWorks EDI 2024 C++ Edition - Sample Project
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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include "../../include/ipworksedi.h"

#define LINE_LEN 256

char input[LINE_LEN + 1];
const char* prompt(const char* prompt, const char* defaultVal) {
  printf("%s [%s]: ", prompt, defaultVal);
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  if (strlen(input) == 0) {
    strncpy(input, defaultVal, LINE_LEN);
    input[LINE_LEN] = '\0';
  }
  return input;
}
const bool yesno(const char* prompt, const bool defaultVal) {
  char def = defaultVal ? 'y' : 'n';
  printf("%s (y/n) [%c]: ", prompt, def);
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  if (strlen(input) == 0) return defaultVal;
  char answer = input[0];
  if (answer == 'y' || answer == 'Y') return true;
  else if (answer == 'n' || answer == 'N') return false;
  else return defaultVal;
}
const char* choice(const char* prompt, const char* category, const char* choices[], const int count, const int defaultIdx) {
  printf("%s:\n", category);
  for (int i = 0; i < count; i++) printf("  %d. %s\n", (i + 1), choices[i]);
  printf("%s [%d]: ", prompt, (defaultIdx + 1));
  fgets(input, LINE_LEN, stdin);
  input[strlen(input) - 1] = '\0';
  int idx = -1;
  if (strlen(input) > 0) int idx = atoi(input) - 1;
  if (idx >= 0 && idx < count) return choices[idx];
  else return choices[defaultIdx];
}

class MyAS2Sender : public AS2Sender {
public:
  static const int sigAlgsCnt = 6;
  static const int encAlgsCnt = 5;
  const char* sigAlgs[sigAlgsCnt] = {"SHA1", "MD5", "SHA-256", "SHA-384", "SHA-512", "SHA-224"};
  const char* encAlgs[encAlgsCnt] = {"3DES", "DES", "AESCBC128", "AESCBC192", "AESCBC256"};

  MyAS2Sender() {}

  virtual int FireSSLServerAuthentication(AS2SenderSSLServerAuthenticationEventParams *e) {
    if (e->Accept) return 0;
    printf("Server provided the following certificate:\nIssuer: %s\nSubject: %s\n", e->CertIssuer, e->CertSubject);
    printf("The following problems have been determined for this certificate: %s\n", e->Status);
    if (yesno("Would you like to continue anyway?", false)) e->Accept = true;
    return 0;
  }
};

int main(int argc, char* argv[]) {
  int ret_code = 0;
  MyAS2Sender as2;

  // Logs will be written to the following directory (for a description of what the macros do, see the documentation).
  as2.SetLogDirectory("./AS2 Logs/To %as2to%/%date%/%messageid%");
  as2.Config("LogOptions=All");

  // The ASP.NET Development Server does not support chunked encoding.
  // If you're posting to an ASP.NET Development Server, uncomment this line.
  //as2.Config("UseChunkedEncoding=false");

  printf("*********************************************************************\n");
  printf("* This demo shows how to use the AS2Sender component to communicate *\n");
  printf("* with an AS2 server. For simplicity's sake, this demo communicates *\n");
  printf("* with Mendelson's public AS2 test servers by default. Feel free to *\n");
  printf("* modify the source if you wish to test against another AS2 server. *\n");
  printf("*                                                                   *\n");
  printf("* For information regarding Mendelson's AS2 test servers, refer to: *\n");
  printf("* http://mendelson-e-c.com/as2/                                     *\n");
  printf("*********************************************************************\n");
  printf("(press any key to continue)");
  getchar();
  printf("\n");

  // Specify the necessary AS2 identifiers and the receiving server's URL.
  as2.SetAS2From(prompt("Sender's AS2 Identifier", "mycompanyAS2"));
  as2.SetAS2To(prompt("Receiver's AS2 Identifier", "mendelsontestAS2"));
  as2.SetURL(prompt("Receiver URL", "http://testas2.mendelson-e-c.com:8080/as2/HttpReceiver"));

  // Specify whether to compress the outgoing message with ZLIB.
  if (yesno("Compress outgoing message?", true))
    as2.SetCompressionFormat(1); // 0 = None, 1 = ZLIB.
  printf("\n");

  // Specify the signing options, if desired.
  bool sign = yesno("Sign outgoing message?", true);
  if (sign) {
    // Specify the signing algorithm to use.
    as2.SetSignatureAlgorithm(choice("Choose the signing algorithm to use", "Signing Algorithms", 
      as2.sigAlgs, as2.sigAlgsCnt, 2)); // Default to SHA-256.

    // Specify the signing certificate information. The signing certificate is the private certificate of the sender.
    as2.SetSigningCertStoreType(2); // .pfx/.p12 private key file.
    prompt("Specify the sender's private key (a .pfx/.p12 file) to use for signing", "./testserver_sender.pfx");
    as2.SetSigningCertStore(input, strlen(input));
    as2.SetSigningCertStorePassword(prompt("Private key password", "test"));
    as2.SetSigningCertSubject(prompt("Private key subject", "*"));
  }
  printf("\n");

  // Specify the encryption options, if desired.
  bool encrypt = yesno("Encrypt outgoing message?", true);
  if (encrypt) {
    // Specify the encryption algorithm to use.
    as2.SetEncryptionAlgorithm(choice("Choose the encryption algorithm to use", "Encryption Algorithms",
      as2.encAlgs, as2.encAlgsCnt, 0)); // Default to 3DES.
    
    // Specify the encryption certificate information. The encryption certificate is the public certificate
    // of the receiver.
    as2.SetRecipientCertCount(1);
    as2.SetRecipientCertStoreType(0, 6); // .cer public key file (PEM-encoded).
    prompt("Specify the receiver's public key (a .cer file) to use for encryption", "./testserver_receiver.cer");
    as2.SetRecipientCertStore(0, input, strlen(input));
    as2.SetRecipientCertSubject(0, prompt("Public key subject", "*"));
  }
  printf("\n");

  // To request an MDN (Message Disposition Notification) based receipt, you should set the MDNTo property to 
  // specify where to deliver the MDN to. (Note that the actual value is  irrelevant, as most servers just check
  // to see whether anything was specified at all).
  bool mdnRequested = strcmp("none", prompt("Deliver MDN to (or \"none\" for no MDN)", "as2@nsoftware.com"));
  bool signedMdn;
  bool separateSignerCert;
  if (mdnRequested) {
    as2.SetMDNTo(input);
    // By default the component will request a SIGNED receipt, with a Received-Content-MIC value that establishes
    // digital non-repudiation. If you prefer to receive an unsigned receipt, set MDNOptions to an empty string.
    signedMdn = yesno("Request signed MDN?", true);
    if (!signedMdn) as2.SetMDNOptions("");
    else {
      // Typically, receivers use the same certificate for both decrypting incoming messages and signing outgoing
      // MDNs, so the AS2Sender component will use the first certificate held by the RecipientCert* properties to
      // both encrypt outgoing data and to verify incoming MDNs.

      // However, some receivers use an additional certificate just for signing outgoing MDNs, in which case we'll
      // need to set the ReceiptSignerCert* properties to let the AS2Sender component know that that's the case.
      // (Alternatively, we might need to set the ReceiptSignerCert* properties simply because we did not wish to
      // encrypt the outgoing message, and therefore did not set the RecipientCert* properties.)

      // Mendelson requires encryption, and uses the same certificate for both decryption and MDN signing, so the
      // defaults here reflect that use-case, but be sure to check with your partner if you use this demo to test.
      separateSignerCert = as2.GetRecipientCertCount() == 0 || 
        yesno("Does the receiver use a separate certificate just for signing MDNs?", false);
      if (separateSignerCert) {
        as2.SetReceiptSignerCertStoreType(6); // .cer public key file (PEM-encoded).
        prompt("Specify the receiver's public key (a .cer file) to use for verifying MDNs", "");
        as2.SetReceiptSignerCertStore(input, strlen(input));
        as2.SetReceiptSignerCertSubject(prompt("Public key subject", "*"));
      }
    }
  }
  printf("\n");
  
  // Specify the data to send. For simplicity's sake, this demo just asks the user to type some data by default.
  // If you wanted to send a file instead, you could set the EDIFileName property.
  prompt("Enter the data to send", "This is a test.");
  as2.SetEDIData(input, strlen(input));
  printf("\n");

  // Print a summary of what's been configured.
  char *temp; int tempLen;
  printf("Sending using the following options:\n");
  printf("AS2From: %s\n", as2.GetAS2From());
  printf("AS2To: %s\n", as2.GetAS2To());
  printf("URL: %s\n", as2.GetURL());
  printf("Sign Message: %s\n", (sign ? "True" : "False"));
  if (sign) {
    printf("Signature Algorithm: %s\n", as2.GetSignatureAlgorithm());
    as2.GetSigningCertStore(temp, tempLen);
    printf("Signing Cert Store: %s\n", temp);
    printf("Signing Cert Password: %s\n", as2.GetSigningCertStorePassword());
    printf("Signing Cert Subject: %s\n", as2.GetSigningCertSubject());
  }
  printf("Encrypt Message: %s\n", (encrypt ? "True" : "False"));
  if (encrypt) {
    printf("Encryption Algorithm: %s\n", as2.GetEncryptionAlgorithm());
    as2.GetRecipientCertStore(0, temp, tempLen);
    printf("Encryption Cert Store: %s\n", temp);
    printf("Encryption Cert Subject: %s\n", as2.GetRecipientCertSubject(0));
  }
  printf("MDN Request Type: %s\n", (mdnRequested ? (signedMdn ? "Signed MDN" : "Unsigned MDN") : "No MDN"));
  if (mdnRequested) {
    printf("Deliver MDN To: %s\n", as2.GetMDNTo());
    if (signedMdn) printf("MDN Options: %s\n", as2.GetMDNOptions());
    if (separateSignerCert) {
      as2.GetReceiptSignerCertStore(temp, tempLen);
      printf("MDN Signer Cert Store: %s\n", temp);
      printf("MDN Signer Cert Subject: %s\n", as2.GetReceiptSignerCertSubject());
    }
  }
  as2.GetEDIData(temp, tempLen);
  printf("Data to send: %s\n", temp);
  printf("\n");

  // Now, we send the data using the Post() method. If the return value of Post() is 0, then the component was
  // able to post the data and verify the response. If you requested a synchronous MDN, then it's automatically
  // validated (this demo doesn't support asynchronous MDNs). If any issues occur, the Post() method will return
  // a non-zero value, and you can query the GetLastErrorCode() and GetLastError() methods for more information.
  printf("Sending data...");
  ret_code = as2.Post();
  if (ret_code) {
    printf("Failed.\nReason: %s\n", as2.GetLastError());
  } else {
    printf("Success! %s\n\n", (mdnRequested ? "MDN Verified" : "No MDN Requested"));
    if (mdnRequested) {
      printf("======================================== MDN Headers ========================================\n");
      printf("%s\n", as2.GetMDNReceiptHeaders());
      printf("=============================================================================================\n\n");
      printf("============================= MDN Message (Human-Readable Part) =============================\n");
      printf("%s\n", as2.GetMDNReceiptMessage());
      printf("=============================================================================================\n\n");
      printf("============================ MDN Message (Machine-Readable Part) ============================\n");
      printf("%s\n", as2.GetMDNReceiptMDN());
      printf("=============================================================================================\n\n");
      printf("====================================== Raw MDN Content ======================================\n");
      char *mdnContent; int mdnContentLen;
      as2.GetMDNReceiptContent(mdnContent, mdnContentLen);
      fwrite(mdnContent, sizeof(char), mdnContentLen, stdout);
      printf("\n");
      printf("=============================================================================================\n\n");
    }
  }

  printf("\n(press any key to exit)");
  getchar();
  return 0;
}

