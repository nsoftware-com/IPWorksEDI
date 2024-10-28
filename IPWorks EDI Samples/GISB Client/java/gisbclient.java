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

public class gisbclient {

	public static void main(String[] args) {
		
		GISBSender gisbsender1 = new GISBSender();

		String homedir = new File("").getAbsolutePath();
		String passphrase = "testsender";
		String userid = "GISBSender";
		String receiverid = "GISBReceiver";		
		
		try {

			// In this demo the internal PGP implementation is used.
			gisbsender1.config("PGPProviderType=1");

			/*
			 * The internal implementation is recommended because there are no
			 * additional dependencies. External implementations are supported
			 * through the provider class (see the gnupg_provider class). The
			 * provider class acts as an interface with a PGP provider, in this
			 * case GnuPG. When configured, all PGP operations are handled by
			 * GnuPG (gpg.exe). To configure the component to use the external
			 * provider set:
			 * 
			 * gisbsender1.config("PGPProviderType=0");
			 * gisbsender1.config("PGPProvider=gnupg_provider");
			 * gisbsender1.setPGPParam("gpg-path", "C:\\Program Files (x86)\\GNU\\GnuPG\\gpg.exe");
			 */

			gisbsender1.setDataFrom("GISBTestSender");
			gisbsender1.setDataTo("GISBTestReceiver");
			gisbsender1.setURL("http://localhost:8080/test/gisbserver.jsp");

			GISBData data = gisbsender1.getGISBData();
			data.setEDIType("X12");
			data.setData("Paste EDI data here.");
			gisbsender1.setGISBData(data);

			// If you set a log directory, the component will produce detailed log files.
			gisbsender1.setLogDirectory("/gisb-logs");

			gisbsender1.setPGPParam("homedir", homedir);

			// set properties for signing
			gisbsender1.setPGPParam("passphrase", passphrase);
			gisbsender1.setPGPParam("userid", userid);
			gisbsender1.setSignData(true);

			// set properties for encryption
			gisbsender1.setPGPParam("recipient-userid", receiverid);
			gisbsender1.setEncryptData(true);

			gisbsender1.post();

			// If the call to post returns without throwing an exception, then
			// the
			// component was able to post the data and get a response.
			System.out.println("Transmission was successful.");
		} catch (IPWorksEDIException exc) {
			System.out.println("Transmission was not successful: " + exc.getMessage());
		} finally {
			System.out.print(new String(gisbsender1.getResponseContent()));

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



