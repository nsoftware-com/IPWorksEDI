/*
 * IPWorks EDI 2024 Java Edition - Sample Project
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

public class as4client {

	public static void main(String[] args) {
		try {
			AS4Client as4client = new AS4Client();

			// Specify the agreement and party information
			as4client.setAgreementRef("http://agreements.company.com/sign_and_encrypt");

			// Specify the AS4 party information
			EBPartyInfo as4from = new EBPartyInfo();
			as4from.setRole("http://docs.oasis-open.org/ebxml-msg/ebms/v3.0/ns/core/200704/initiator");
			as4from.setId("org:b2b:example:company:A");
			as4client.setAS4From(as4from);

			EBPartyInfo as4to = new EBPartyInfo();
			as4to.setRole("http://docs.oasis-open.org/ebxml-msg/ebms/v3.0/ns/core/200704/responder");
			as4to.setId("org:b2b:example:company:B");
			as4client.setAS4To(as4to);

			// Note that setting a signing certificate will instruct the class to
			// sign the message. Leave these
			// properties blank to send an unsigned message.
			as4client.setSigningCert(new Certificate(Certificate.cstPFXFile,"as4client.pfx", "test", "*"));

			// Similarly, setting the recipient's certificate will instruct the
			// class to encrypt the message.
			as4client.getRecipientCerts().add(new Certificate("as4server.cer"));

			// There is also a SignerCert property, which you should set if your
			// trading partner uses different certs for signing and encryption.
			as4client.setSignerCert(new Certificate("as4server.cer"));

			// If you set a log directory, the class will produce detailed log
			// files.
			as4client.setLogDirectory("/as4client-logs");

			// Set your partner's URL (HTTP or HTTPS).
			as4client.setURL("http://localhost:8080/test/as4server.jsp");

			// Specify the EDI payload
			EBData data = new EBData();
			data.setEDIType("text/xml");
			data.setData("<example-document><content>This is just a very simple XML document to show transport of XML payloads in the SOAP body</content></example-document>".getBytes());
			data.setName("myfile.xml");
			as4client.getEDIData().add(data);
			
			//Optionally specify an AS4 profile. This specifies different defaults based on requirements for the profile
			//as4client.setProfile(as4client.ebpfENTSOG);

			// Configure the component to expect a synchronous receipt
			as4client.setReceiptReplyMode(AS4Client.rrmSync);

			// If the call to sendFiles() returns without throwing an exception,
			// then the class was able to post
			// the data and verify the response. In particular, if you requested
			// a synchronous receipt,
			// it will automatically be validated, and an exception will be
			// thrown if there are any problems.

			// If you requested an asynchronous receipt, details about the
			// original message must be stored so
			// that the receipt can be correlated with the message and properly
			// verified. The easiest way to do
			// this is to set AsyncReceiptInfoDir before calling SendFiles. The
			// class will automatically store
			// the required information. See the VerifyReceipt method of
			// AS4Server for details about verifying
			// asynchronous receipts.
			as4client.sendFiles();
			System.out.println("Message sent successfully. Receipt contents:\r\n");
			System.out.println(new String(as4client.getReceipt().getContent()));
		} catch (IPWorksEDIException ex) {
			System.out.println("Error: " + ex.getMessage());
		}
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
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
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

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



