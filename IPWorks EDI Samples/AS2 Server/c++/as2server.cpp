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
#include "../../include/ipworksedi.h"

#define SENDER_ID "My_AS2_Sender"
#define SENDER_PUBLIC_CERT "./as2sender.cer"
#define SENDER_PRIVATE_CERT "./as2sender.pfx"
#define RECEIVER_ID "My_AS2_Receiver"
#define RECEIVER_PUBLIC_CERT "./as2receiver.cer"
#define RECEIVER_PRIVATE_CERT "./as2receiver.pfx"
#define EDI_DATA "This is a test."

int main(int argc, char* argv[]) {
  int ret_code = 0;
  AS2Sender as2Sender;
  AS2Receiver as2Receiver;

  // Logs will be written to the following directory (for a description of what the macros do, see the documentation).
  as2Sender.SetLogDirectory("./AS2 Logs/%date%/To %as2to%/%messageid%");
  as2Sender.Config("LogOptions=All");
  as2Receiver.SetLogDirectory("./AS2 Logs/%date%/From %as2from%/%messageid%");
  as2Receiver.Config("LogOptions=All");

  printf("*********************************************************************\n");
  printf("* This demo shows how the AS2Receiver component can be utilized for *\n");
  printf("* AS2 request processing. The AS2Receiver component doesn't include *\n");
  printf("* a built-in webserver, so the AS2Sender component is used to build *\n");
  printf("* an AS2 request (which is then passed to AS2Receiver), and then to *\n");
  printf("* verify the MDN that AS2Reciever generates. To get the most out of *\n");
  printf("* this demo, please refer to the comments in the source code, which *\n");
  printf("* detail how the AS2Receiver component is used.                     *\n");
  printf("*********************************************************************\n");
  printf("(press any key to continue)");
  getchar();
  printf("\n");

  // For the purpose of the demo, we'll use our AS2Sender component to "send" an AS2 request to a file on disk. Please
  // refer to the "as2client" demo for a much more detailed explanation of what all of the options we're using below do.
  as2Sender.SetAS2From(SENDER_ID);
  as2Sender.SetAS2To(RECEIVER_ID);
  as2Sender.SetCompressionFormat(1);
  as2Sender.SetSignatureAlgorithm("SHA-256");
  as2Sender.SetSigningCertStoreType(2);
  as2Sender.SetSigningCertStore(SENDER_PRIVATE_CERT, strlen(SENDER_PRIVATE_CERT));
  as2Sender.SetSigningCertStorePassword("test");
  as2Sender.SetSigningCertSubject("*");
  as2Sender.SetEncryptionAlgorithm("3DES");
  as2Sender.SetRecipientCertCount(1);
  as2Sender.SetRecipientCertStoreType(0, 6);
  as2Sender.SetRecipientCertStore(0, RECEIVER_PUBLIC_CERT, strlen(RECEIVER_PUBLIC_CERT));
  as2Sender.SetRecipientCertSubject(0, "*");
  as2Sender.SetMDNTo("as2@nsoftware.com");
  as2Sender.SetEDIData(EDI_DATA, strlen(EDI_DATA));
  // We use dummy value for the next two properties so that the AS2Sender will request an "asynchronous" MDN. This works
  // well for this demo since we aren't actually using real HTTP requests, and thus have to shuttle the data between the
  // two components ourselves.
  as2Sender.SetMDNDeliveryOption("http://async/");
  as2Sender.SetAsyncMDNInfoDir("./AS2SenderTemp");
  printf("Writing request to file using AS2Sender...");
  as2Sender.Config("PostToFile=./AS2MessageFromSender.dat");
  printf("Done.\n\n");

  // The first thing to do with AS2Receiver is load the request you wish for it to process into the Request property
  // (if your request headers are separate from your request body, you can put them into the RequestHeadersString
  // property). As mentioned above, we wrote our whole AS2 request to a file for the purpose of this demo, so we'll
  // read that content into AS2Reciever now (though we'll use the RequestFile config for convenience).
  printf("Loading request from file into AS2Receiver...");
  as2Receiver.Config("RequestFile=./AS2MessageFromSender.dat");
  printf("Done.\n\n");

  // Now that the AS2Receiver has the request, we need to read it. AS2Receiver provides a variety of methods to give
  // you various degrees of control over the process of receiving, parsing, verifying, and finally responding to AS2
  // messages. This demo only shows one possible way to proceed through those steps, and it's highly recommended that
  // you refer to the documentation for the AS2Receiver component, which details alternative approaches.

  // To start, we'll read the request's headers using ReadRequest(). This populates a variety of properties, and in a
  // real-like scenario you could then use business logic to do things such as checking that the message was intended
  // for you, restricting further processing to specific senders, loading specific certificates, etc. Since our demo
  // is self-contained, we'll just print the information and move on.
  printf("Reading request headers...");
  as2Receiver.ReadRequest();
  printf("Done.\n");
  // Print the raw request headers.
  printf("Raw Request Headers:\n");
  for (int i = 0; i < as2Receiver.GetRequestHeaderCount(); i++)
    printf("%s=%s\n", as2Receiver.GetRequestHeaderField(i), as2Receiver.GetRequestHeaderValue(i));
  // Print the request details using the populated properties.
  printf("\nRequest Details:\n");
  printf("Sender's AS2 Identifier: %s\n", as2Receiver.GetAS2From());
  printf("Intended Receiver's AS2 Identifier: %s\n", as2Receiver.GetAS2To());
  printf("Incoming AS2 Version: %s\n", as2Receiver.Config("AS2VersionIncoming"));
  printf("Incoming Message Id: %s\n", as2Receiver.GetMessageId());
  printf("Deliver MDN To: %s\n", as2Receiver.GetMDNTo());
  printf("Receipt Delivery Option: %s\n", as2Receiver.GetReceiptDeliveryOption());
  printf("Requested Signature Protocol: %s\n", as2Receiver.Config("RequestedSignatureProtocol"));
  printf("Requested MIC Algorithms: %s\n", as2Receiver.Config("RequestedMICAlgorithms"));
  printf("\n");

  // Before we parse the request, we'll need to set the certificates to use for decryption and signature verification.
  // The Cert* properties are used to specify the certificate for decryption (and signing the MDN, if necessary), and
  // should be set to the receiver's (this is, _your_) private key (a .pfx or .p12 file); the SignerCert* properties
  // are used to specify the certificate for verifying the sender's signature, and should be set to the sender's public
  // key (a .cer file).
  as2Receiver.SetCertStoreType(2); // a .pfx or .p12 file.
  as2Receiver.SetCertStore(RECEIVER_PRIVATE_CERT, strlen(RECEIVER_PRIVATE_CERT));
  as2Receiver.SetCertStorePassword("test");
  as2Receiver.SetCertSubject("*");
  as2Receiver.SetSignerCertStoreType(6); // a .cer file (PEM-encoded).
  as2Receiver.SetSignerCertStore(SENDER_PUBLIC_CERT, strlen(SENDER_PUBLIC_CERT));
  as2Receiver.SetSignerCertSubject("*");

  // Now that we have the correct certificates set up, we can use the ParseRequest() method to parse the request body.
  // (Note that if we hadn't already called ReadRequest(), it would also parse the request headers too.)
  
  // If ParseRequest() was successful in reading the request body (decompressing, decrypting, and verifying it along
  // the way, as necessary), then the Request property will have been cleared, and the EDIData property will now hold
  // the parsed request body's content. In addition, a few configs will have been populated with more information
  // about the request itself (such as the encryption type used, and other such information).
  
  // If there was an issue, the Request and EDIData properties remain unchanged, the ParseRequest() method will have
  // returned a non-zero code, and you can query ScanResult to determine an AS2-specific reason for why the request
  // parsing failed. You can still generate an MDN in this case, it will just contain details about the error.

  // So, now we'll parse the request and print the ScanResult.
  printf("Parsing request body...");
  as2Receiver.ParseRequest();
  printf("Done.\n");
  // ScanResult should always be 0, which indicates success, in this demo. Refer to the documentation for descriptions
  // of the other possible return values.
  printf("Scan Result: %d\n", as2Receiver.GetScanResult());
  printf("Encryption Type: %s\n", as2Receiver.Config("EncryptionType"));
  printf("Signature Type: %s\n", as2Receiver.Config("SignatureType"));
  printf("Compression Type: %s\n\n", (as2Receiver.GetCompressionFormat() ? "ZLIB" : ""));
  printf("====================================== Parsed EDI Data ======================================\n");
  char *ediData; int ediDataLen;
  as2Receiver.GetEDIData(ediData, ediDataLen);
  fwrite(ediData, sizeof(char), ediDataLen, stdout);
  printf("\n");
  printf("=============================================================================================\n\n");

  // Now that we've parsed the request completely, we'll create an MDN receipt using CreateMDNReceipt().
  // This will populate the MDN* properties.
  printf("Creating MDN receipt...");
  as2Receiver.CreateMDNReceipt("", "", "");
  printf("Done.\n\n");

  // We'll have to simulate sending the MDN back to the sender, since AS2Receiver isn't running within a web server here.
  printf("Transferring MDN from AS2Receiver to AS2Sender...");
  char *mdnContent; int mdnContentLen;
  as2Receiver.GetMDNReceiptContent(mdnContent, mdnContentLen);
  as2Sender.SetMDNReceiptContent(mdnContent, mdnContentLen);
  as2Sender.SetMDNReceiptHeaders(as2Receiver.GetMDNReceiptHeaders());
  printf("Done.\n\n");

  // Now, we'll have AS2Sender verify the MDN receipt. Note that we configured AS2Sender to "request" an asynchronous MDN
  // originally, otherwise it wouldn't have kept ahold of the information that it needs to verify the MDN (in synchronous
  // mode, this is fine, since any MDN would have come back in the response).
  printf("Verifying MDN receipt...");
  ret_code = as2Sender.VerifyReceipt();
  if (ret_code) {
    printf("Failed\nReason: %s\n", as2Sender.GetLastError());
  } else {
    printf("Success!\n\n");
    printf("======================================== MDN Headers ========================================\n");
    printf("%s\n", as2Sender.GetMDNReceiptHeaders());
    printf("=============================================================================================\n\n");
    printf("============================= MDN Message (Human-Readable Part) =============================\n");
    printf("%s\n", as2Sender.GetMDNReceiptMessage());
    printf("=============================================================================================\n\n");
    printf("============================ MDN Message (Machine-Readable Part) ============================\n");
    printf("%s\n", as2Sender.GetMDNReceiptMDN());
    printf("=============================================================================================\n\n");
    printf("====================================== Raw MDN Content ======================================\n");
    as2Sender.GetMDNReceiptContent(mdnContent, mdnContentLen);
    fwrite(mdnContent, sizeof(char), mdnContentLen, stdout);
    printf("\n");
    printf("=============================================================================================\n\n");
  }

  printf("\n(press any key to exit)");
  getchar();
  return 0;
}

