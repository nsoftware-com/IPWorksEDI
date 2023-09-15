/*
 * IPWorks EDI 2022 Java Edition - Sample Project
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

import java.io.*;
import ipworksedi.*;

public class as1 extends ConsoleDemo {
	
	private String logDir = "";
	private String myAddress = "";
	private String PFXFilePath = "";
	private String PFXPassword = "";
	private String certSubject = "";
	private String pubKey = "";
	private String popUser = "";
	private String popPasswd = "";
	
	// If you use this demo to run a self-test of the entire AS1 process, you will need to store these
	// values after sending, so that the MDN can be verified.

	// In a realistic trading partner situation you should not count on receiving a response
	// immediately, so you should store these externally in a database (where you can look them up
	// by MessageId).	
	private String originalContentMIC = "";
	private String messageId = "";
	private String mdnOptions = "";
	
	public void sendMessage(boolean sign, boolean encrypt, boolean compress,
			boolean requestMDN, boolean requestMDNSignature) throws IPWorksEDIException, java.io.IOException {

		
		String response;
		As1sender as1sender = new As1sender();

		// First, configure your mail server
		as1sender.setMailServer(prompt("Specify the SMTP server",":","localhost"));
		
		myAddress = prompt("Specify From email address ", ":", "from@localhost.com");
		as1sender.setFrom("\"AS1 Java Sender\" <" + myAddress + ">");
		
		response = prompt("Specify To email address ",":","to@localhost.com");
		as1sender.setSendTo("\"AS1 Java Receiver\" <" + response + ">");
		
		as1sender.setSubject(prompt("Specify email subject ",":","AS1 test message"));

		response = prompt("Specify EDI data ",":","test");
		as1sender.setEDIData(new EDIData(response.getBytes(), "application/edi-x12"));

		logDir = prompt("Specify log directory ",":","java-as1-logs-send");
		as1sender.setLogDirectory(logDir);

		if (sign || encrypt) { 
			PFXFilePath = prompt("Specify the path to your PFX or PEM File",":","./testcert.pfx");
			PFXPassword = prompt("Specify password, if applicable",":","password");
			
			certSubject = prompt("Specify cert subject",":","*");
			
			if (PFXFilePath.substring(PFXFilePath.length()-3).toLowerCase().equals("pfx"))
			{
				as1sender.setSigningCert(new Certificate(Certificate.cstPFXFile, PFXFilePath,
						PFXPassword, certSubject));
			}
			else if (PFXFilePath.substring(PFXFilePath.length()-3).toLowerCase().equals("pem"))
			{
				as1sender.getRecipientCerts().add(new Certificate(pubKey));
			}
		}

		if (compress)
			as1sender.setCompressionFormat(As1sender.cfZLIB);

		if (requestMDN) {
			as1sender.setMDNTo(myAddress);
			// The default is to request a signature on the MDN.
			if (! requestMDNSignature)
				as1sender.setMDNOptions("");
		}

		as1sender.send();
		System.out.println("Send is finished.");

		originalContentMIC = as1sender.getOriginalContentMIC();
		messageId = as1sender.getMessageId();
		mdnOptions = as1sender.getMDNOptions();
	}

	public void receiveMessage() throws IPWorksEDIException, java.io.IOException {
		String response;
		As1receiver as1receiver = new As1receiver();

		as1receiver.setLogDirectory(logDir);

		as1receiver.setMailServer(prompt("Specify POP server to fetch the AS1 message from ",":"));

		as1receiver.setUser(prompt("Specify user name ",":"));
		
		as1receiver.setPassword(prompt("Specify password ",":"));
		as1receiver.connect();

		// Assume that the last message in the mailbox is the AS1 message. In a real
		// AS1 server you would use the other MailMessage properties to navigate the inbox
		// and look for AS1 messages. The component can in fact be used to write a fairly
		// complete POP client.

		as1receiver.setMailMessageNumber(as1receiver.getMailMessageCount());
		as1receiver.readRequest();

		as1receiver.disconnect();

		// Set certificates for yourself and your trading partner (unless you are for sure
		// not expecting encrypted or signed messages
			
			PFXFilePath = prompt("Specify decryption certificate (.pfx file)",":","./testcert.pfx");
			
			PFXPassword = prompt("Specify password",":","password");
			
			response = prompt("Specify cert subject",":","*");
			
			as1receiver.setCertificate(new Certificate(Certificate.cstPFXFile, PFXFilePath,
					PFXPassword, certSubject));
			

			
		response = prompt("Specify your trading partners public key (.cer file)",":","./testcert.cer");
		as1receiver.setSignerCert(new Certificate(response));

		// Process the request and generate an MDN receipt.
		as1receiver.processRequest();

		// At this point you can read the SignatureType, EncryptionType, etc. properties to determine
		// what security was applied to the message, and you can read the EDIData and EDIType files to
		// get at the data.

		// Unless you don't like the data or security parameters, you should send a receipt in response.
		// Note that if your trading partner didn't request a receipt this will default to a noop.

		as1receiver.sendResponse();
	}

	public void processReceipt() throws IPWorksEDIException, java.io.IOException {
		String response;
		// Use the sender component to process receipts. (This is parallel to the AS2 case, where the
		// receipts are returned synchronously, with no extra processing required.
		As1sender as1sender = new As1sender();

		as1sender.setLogDirectory(logDir);

		// (see comments in receiveMessage.)
		as1sender.setMailServer(prompt("Specify POP server to fetch the AS1 MDN from",":"));

		as1sender.setUser(prompt("Specify user name",":"));
		
		as1sender.setPassword(prompt("Specify password",":"));
		
		as1sender.connect();
		as1sender.setMailMessageNumber(as1sender.getMailMessageCount());
		as1sender.readReceipt();		
		as1sender.disconnect();

		// This code assumes that the receiver side will sign receipts with the same certificate used
		// for encryption. This is easiest, but not required.
		response = prompt("Specify your trading partners public key (.cer file)",":","./testcert.cer");
		as1sender.setReceiptSignerCert(new Certificate(response));

		as1sender.readReceipt();
		// At this point you can read the SendTo property. This will correspond to the recipient \
		// of the original message, i.e., the sender of the receipt. You can also read the MessageId
		// property, which will let you look up the OriginalContentMIC property from wherever you
		// stored it when you sent the message.

		as1sender.setOriginalContentMIC(this.originalContentMIC);
		as1sender.setMDNOptions(this.mdnOptions);

		as1sender.verifyReceipt();
	}


	public void test() {
		String response;		
		boolean sign = false;
		boolean encrypt = false;
		boolean compress = false;
		boolean requestMDN = false;
		boolean requestMDNSigned = false;
		char answer;

		answer = ask("Do you want to sign outgoing message",":");
		if(answer == 'y') sign = true;

		answer = ask("Do you want to encrypt outgoing message",":");
		if(answer == 'y') encrypt = true;

		answer = ask("Do you want to compress outgoing message",":");
		if(answer == 'y') compress = true;

		answer = ask("Do you want to request MDN",":");
		if(answer == 'y') requestMDN = true;

		if( requestMDN ) {
			answer = ask("Do you want the MDN to be signed",":");
			if(answer == 'y') requestMDNSigned = true;
		}

		try{
			sendMessage(sign, encrypt, compress, requestMDN, requestMDNSigned);
			receiveMessage();
			if (requestMDN)
				processReceipt();
			System.out.println("Transaction completed.");
		}
		catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public static void main(String[] foo) {
		new as1().test();
	}
}
class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }

  static String prompt(String label, String punctuation, String defaultVal)
  {
	System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
	String response = input();
	if(response.equals(""))
		return defaultVal;
	else
		return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksEDIException) {
      System.out.print(" (" + ((IPWorksEDIException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }
}



