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
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import ipworksedi.*;

public class certmgr {
    CertMgr certmgr1;
    int i = 0;
    public certmgr(){
        try
        {
            final String defaultCertStoreFile = "myidentities.jks";
            final String defaultCertStorePassword = "password";
            String filename = "";
            certmgr1 = new CertMgr();
            certmgr1.addCertMgrEventListener(new CertMgrEvents(this));
            String buffer; //user input
            // read a cert store file
            System.out.print("Please enter java key store (.jks) path [\""+defaultCertStoreFile+"\"]: ");
            buffer = input();
            filename = (buffer.length()>0)?buffer:defaultCertStoreFile;
            certmgr1.setCertStoreType(certmgr1.cstJKSFile); //user java key store (JKS file)
            certmgr1.setCertStore(filename);

            System.out.print("Please enter store password [\""+defaultCertStorePassword+"\"]: ");
            buffer = input();
            // This demo doesn't allow an empty string as a password for the .jks file.
            // If it is an empty string, a string "password" will be used.
            if (buffer.length() > 0)
            {
              certmgr1.setCertStorePassword(buffer);
            }
            else
            {
              certmgr1.setCertStorePassword(defaultCertStorePassword);
            }
            certmgr1.listStoreCertificates();
        }catch(IPWorksEDIException ex){
            System.out.println("IPWorks exception thrown: " + ex.getCode() + " [" + ex.getMessage() + "].");
        }
        catch(Exception ex){
            System.out.println(ex.getMessage());
        }
        System.out.println("Exited.");
    }
    public static void main(String[] args) {
	new certmgr();
    }
    private String input() throws IOException
    {
        BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
        return bf.readLine();
    }
    public void certList(CertMgrCertListEvent args) {
        i++;
        System.out.println(i + ". " + args.certSubject);
    }
}
class CertMgrEvents implements CertMgrEventListener{
		certmgr instance;
    public CertMgrEvents(certmgr instance){
        this.instance = instance;
    }
    public void certChain(CertMgrCertChainEvent args) {
    }
    public void certList(CertMgrCertListEvent args) {
        instance.certList(args);
    }
    public void error(CertMgrErrorEvent args) {
    }
    public void keyList(CertMgrKeyListEvent args) {
    }
    public void storeList(CertMgrStoreListEvent args) {
    }
    public void log(CertMgrLogEvent arg0) {
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
      System.out.print(label + " [" + defaultVal + "] " + punctuation + " ");
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
}



