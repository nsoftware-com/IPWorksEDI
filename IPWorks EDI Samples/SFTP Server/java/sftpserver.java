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
import java.util.*;
import java.lang.*;
import ipworksedi.*;

public class sftpserver extends ConsoleDemo{
	public static class myFTPServer extends SFTPServer {
		
		public myFTPServer() {
			super();
			try {
				addSFTPServerEventListener(new ipworksedi.SFTPServerEventListener() {

					public void SSHStatus(SFTPServerSSHStatusEvent e) {	
						Log(e.connectionId,e.message);
					}
					public void SSHUserAuthRequest(SFTPServerSSHUserAuthRequestEvent e) {	
						if(e.user.equals("test") && !e.authMethod.equals("none") && e.authParam.equals("test"))
						{
							e.accept = true;
							Log(e.user + " has successfully authenticated.");
						}
					}
					public void connected(SFTPServerConnectedEvent e) {	
						Log(e.connectionId,"Now Connected");
					}
					public void connectionRequest(SFTPServerConnectionRequestEvent e) {
						Log(e.address + ":" + String.valueOf(e.port) + " is attempting to connect.");
					}
					public void dirCreate(SFTPServerDirCreateEvent e) {	
						Log(e.user + " created the directory " + e.path);
					}
					public void dirList(SFTPServerDirListEvent e) {	
					}
					public void dirRemove(SFTPServerDirRemoveEvent e) {	
						Log(e.user + " deleted the directory " + e.path);
					}
					public void disconnected(SFTPServerDisconnectedEvent e) {	
						Log(e.connectionId, "Now Disconnected");
					}
					public void error(SFTPServerErrorEvent e) {	
						Log(e.connectionId, "Error: " + e.description);
					}
					public void fileClose(SFTPServerFileCloseEvent e) {	
						Log(e.user + " transferred " + e.path);
					}
					public void fileOpen(SFTPServerFileOpenEvent e) {	
						String operation = "";
						if((e.flags & 1) != 0) //Read
							operation = "downloading";
						if((e.flags % 2) != 0)
							operation = "uploading";
						
						Log(e.user + " started " + operation + " " + e.path);
					}
					public void fileRead(SFTPServerFileReadEvent e) {	
					}
					public void fileRemove(SFTPServerFileRemoveEvent e) {	
						Log(e.user + " deleted the file " + e.path);
					}
					public void fileRename(SFTPServerFileRenameEvent e) {
						Log(e.user + " renamed the file " + e.path);
					}
					public void fileWrite(SFTPServerFileWriteEvent e) {	
					}
					public void getAttributes(SFTPServerGetAttributesEvent e) {	
					}
					public void resolvePath(SFTPServerResolvePathEvent e) {	
					}
					public void setAttributes(SFTPServerSetAttributesEvent e) {
					}
					@Override
					public void log(SFTPServerLogEvent arg0) {
					}
				});
				
			} catch (TooManyListenersException e) {
				e.printStackTrace();
			}
		}
	}
	
	private static void Log(String message)
	{
		System.out.println(message);
	}
	
	private static void Log(String connectionId, String message)
	{
		Log("[" + connectionId + "] " + message);
	}
	
	private static myFTPServer sftpserver1 = null;

	public static void main(String[] args) {
		sftpserver1 = new myFTPServer();

		try {
			System.out.println("**********************************************************");
			System.out.println("* This demo shows how to use the SFTPServer component to *");
			System.out.println("* create a simple SFTP Server.                           *");
			System.out.println("* Use the following credentials to connect.              *");
			System.out.println("* User: test                                             *");
			System.out.println("* Password: test                                         *");
			System.out.println("**********************************************************");
			
			//For the purposes of this demo we are using the included certificate. You may change this line to specify your own certificate.
			sftpserver1.setSSHCert(new ipworksedi.Certificate(Certificate.cstPFXFile, "sftpserver.pfx", "demo", "*"));
			
			sftpserver1.setRootDirectory(prompt("Root Directory", ":", "./"));
			sftpserver1.setLocalPort(Integer.parseInt(prompt("Port", ":", "22")));
			sftpserver1.startListening();
			
			System.out.println("Server listening on port " + sftpserver1.getLocalPort() + ".");
			System.out.println("Press Q to exit.\r\n\r\n");
			
			while(true)
			{
				if(System.in.available()>0)
				{
					if(String.valueOf(read()).equalsIgnoreCase("Q"))
					{
						System.out.println("Server shutting down. Goodbye!");
						sftpserver1.shutdown();
					}
				}
			}
			
		} catch (IPWorksEDIException e) {
			displayError(e);
		} catch (Exception e) {
			displayError(e);
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



