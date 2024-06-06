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

public class oftpclient {
	
	public oftpclient(){
		OFTPClient oftp = new OFTPClient();
		String buffer = "";
		try{
			oftp.addOFTPClientEventListener(new OftpEvents(this));
			oftp.setOverwrite(true);
			
			System.out.println("Server: ");
			buffer = input();
			oftp.setRemoteHost(buffer);
			
			System.out.println("Server SSID: ");
			buffer = input();
			oftp.setServerSSIDCode(buffer);
			
			System.out.println("Server SFID: ");
			buffer = input();
			oftp.setServerSFIDCode(buffer);
			
			System.out.println("Server Password: ");
			buffer = input();
			oftp.setServerPassword(buffer);
			
			System.out.println("Client SSID: ");
			buffer = input();
			oftp.setClientSSIDCode(buffer);
			
			System.out.println("Client SFID: ");
			buffer = input();
			oftp.setClientSFIDCode(buffer);
			
			System.out.println("Client Password: ");
			buffer = input();
			oftp.setClientPassword(buffer);
			
			System.out.println("Would you like to [R]eceive or [S]end?: ");
			buffer = input();
			
			if(buffer.equals("r") || buffer.equals("R"))
			{
				System.out.println("Please enter the download directory: ");
				buffer = input();
				oftp.setDownloadDirectory(buffer);
				oftp.receiveFiles();
				System.out.println("done");
			}
			else
			{
				String localFile = "";
				System.out.println("Please enter the local file to upload: ");
				buffer = input();
				localFile = buffer;
				System.out.println("Please enter the virtual file name: ");
				buffer = input();
				oftp.sendFile(localFile,buffer);
				System.out.println("done");
			}
		}
		catch(IPWorksEDIException exc)
		{
			System.out.println("Exception: "+ exc.getMessage());
		}
		catch(Exception exc)
		{
			System.out.println("Exception: "+ exc.getMessage());
		}
	}
	
	public static void main(String[] args) {
		try{
			new oftpclient();
		}catch (Exception exc){
			System.out.println("Exception: "+ exc.getMessage());
		}
	}
	
    private String input() throws IOException
    {
        BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
        return bf.readLine();
    }
    
    public void endTransfer(OFTPClientEndTransferEvent arg){
    	System.out.println("");
    }
    public void error(OFTPClientErrorEvent arg){
    	System.out.print("ERROR! " + arg.description);
    }
    public void startTransfer(OFTPClientStartTransferEvent arg){
    	System.out.print((arg.direction == 0 ? "Sending " : "Receiving ") + arg.virtualFileName);
    }
    public void transfer(OFTPClientTransferEvent arg){
    	System.out.print(". ");
    }
}

class OftpEvents implements OFTPClientEventListener{
		oftpclient instance;
	public OftpEvents(oftpclient instance)
	{
		this.instance = instance;
	}
	public void acceptFile(OFTPClientAcceptFileEvent arg) {}
	public void endTransfer(OFTPClientEndTransferEvent arg){
		instance.endTransfer(arg);
	}
	
	public void error(OFTPClientErrorEvent arg){
		instance.error(arg);
	}
	
	public void startTransfer(OFTPClientStartTransferEvent arg){
		instance.startTransfer(arg);
	}
	public void transfer(OFTPClientTransferEvent arg){
		instance.transfer(arg);
	}
	public void SSLStatus(OFTPClientSSLStatusEvent arg){
	}	
	public void SSLServerAuthentication(OFTPClientSSLServerAuthenticationEvent arg){
	}	
	public void endResponse(OFTPClientEndResponseEvent arg){
	}
	public void certificateReceived(OFTPClientCertificateReceivedEvent arg){
	}
	public void PITrail(OFTPClientPITrailEvent arg)
	{
		//This event provides detailed logging of the interaction between the client and server
	}
    public void log(OFTPClientLogEvent arg){}
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



