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

                 

public class ftp
{
    FTP ftp1;
    public int verbose;
    long transtime;
    long transbytes;
    String command;               // user's command
    String[] argument;            // arguments to the user's command
    String pathname;              // for use with the ls command
    public ftp(String[] args){
        try
        {
            ftp1 = new FTP();
            ftp1.addFTPEventListener(new FTPEvents(this));
        }catch (Exception ex)
        {
            System.out.println(ex.getMessage());
        }
        if (args.length >= 3)
        {
            try
            {
                ftp1.setRemoteHost(args[0]);
                ftp1.setUser(args[1]);
                ftp1.setPassword(args[2]);
                ftp1.logon();
            }
            catch(IPWorksEDIException e)
            {
                System.out.println( e.getMessage());
                System.exit(e.getCode());
                return;
            }
        }

        if (args.length == 1)
        {
            try
            {
                ftp1.setRemoteHost(args[0]);
                logon();
            }
            catch(IPWorksEDIException e)
            {
                System.out.println(e.getMessage());
                System.exit(e.getCode());
                return;
            }
        }

        while (true)
        {
            try
            {
                ftp1.setRemoteFile("");
                System.out.print( "ftp> " );
                command = input();
                argument = command.split("\\s");

                if ( argument[0].equals("?") || argument[0].equals("help") )
                {
                    System.out.println("?        bye     help     put     rmdir");
                    System.out.println("append   cd      ls       pwd     verbose");
                    System.out.println("ascii    close   mkdir    quit");
                    System.out.println("binary   get     open     rm");
                }
                else if ( argument[0].equals("append") )
                {
                    ftp1.setLocalFile(argument[1]);
                    ftp1.setRemoteFile(argument[2]);
                    ftp1.append();
                }
                else if ( argument[0].equals("ascii") )
                {
                    ftp1.changeTransferMode(1);
                }
                else if ( argument[0].equals("binary") )
                {
                    ftp1.changeTransferMode(2);
                }
                else if ( argument[0].equals("bye") || argument[0].equals("quit"))
                {
                    ftp1.logoff();
                    System.exit(0);
                    return;
                }
                else if (argument[0].equals("close"))
                {
                    ftp1.logoff();
                }
                else if ( argument[0].equals("cd") )
                {
                    if( argument.length > 0)
                        ftp1.changeRemotePath(argument[1]);
                }
                else if ( argument[0].equals("get") )
                {
                    if ( argument.length < 3 )
                    {
                        System.out.println("get command requires remotefile and localfile.");
                    }
                    else
                    {
                        ftp1.setRemoteFile(argument[1]);
                        ftp1.setLocalFile(argument[2]);
                        ftp1.download();
                        updateTime();
                    }
                }
                else if ( argument[0].equals("ls") )
                {
                    if ( argument.length > 1 )
                    {
                        pathname = ftp1.queryRemotePath();
                        ftp1.changeRemotePath(argument[1]);
                        ftp1.listDirectoryLong();
                        ftp1.changeRemotePath(pathname);
                    }
                    else
                        ftp1.listDirectoryLong();
                }
                else if ( argument[0].equals("mkdir") )
                {
                    if ( argument.length > 1 )
                        ftp1.makeDirectory(argument[1]);
                }
                else if ( argument[0].equals("mv") )
                {
                    ftp1.setRemoteFile(argument[1]);
                    ftp1.renameFile(argument[1]);
                }
                else if ( argument[0].equals("open") )
                {
                    if (argument.length < 2) {
                        System.out.println("open command requires following hostname.");
                    } else {
                        ftp1.logoff();
                        ftp1.setRemoteHost(argument[1]);
                        logon();
                    }
                }
                else if ( argument[0].equals("passive") )
                {
                    if ( argument.length > 1 )
                    {
                        if ((argument[1].equals("on")) && !ftp1.isPassive())
                        {
                            ftp1.setPassive(true);
                            System.out.println( "Passive mode ON." );
                        }
                        else if ((argument[1].equals("off")) && ftp1.isPassive())
                        {
                            ftp1.setPassive(false);
                            System.out.println( "Passive mode OFF." );
                        }
                    }
                }
                else if ( argument[0].equals("put") )
                {
                    if ( argument.length < 3 )
                    {
                        System.out.println("put command requires localfile and remotefile.");
                    }
                    else
                    {
                        // put localfile remotefile
                        ftp1.setRemoteFile(argument[2]);
                        ftp1.setLocalFile(argument[1]);
                        ftp1.upload();
                        updateTime();
                    }
                }
                else if ( argument[0].equals("pwd") )
                {
                    System.out.println( ftp1.queryRemotePath() );
                }
                else if ( argument[0].equals("rm") )
                {
                    if ( argument.length > 1 )
                        ftp1.deleteFile(argument[1]);
                }
                else if ( argument[0].equals("rmdir") )
                {
                    if ( argument.length > 1 )
                        ftp1.removeDirectory(argument[1]);
                }
                else if ( argument[0].equals("verbose") )
                {
                    if ( argument.length > 1 )
                    {
                        if ((argument[1].equals("on")) && verbose == 0)
                        {
                            toggle_verbose();
                        }
                        else if ((argument[1].equals("off")) && verbose == 1)
                        {
                            toggle_verbose();
                        }
                    }
                    else
                    {
                        toggle_verbose();
                    }
                }
                else if ( argument[0].equals("") )
                {
                    // Do nothing
                }
                else {
                    System.out.println( "Bad command / Not implemented in demo." );
                } // end of command checking
            }
            catch(IPWorksEDIException e)
            {
                System.out.println(e.getMessage());
                e.printStackTrace();
                System.exit(e.getCode());
                return;
            }
            catch(IOException e)
            {
                System.out.println(e.getMessage());
            }
        }
    }
    private void updateTime()
    {
        System.out.print(transbytes);
        System.out.print(" bytes sent in ");
        System.out.print(((float) transtime / 1000));
        System.out.print(" seconds.  (");
        System.out.print(((float) transbytes) / transtime);
        System.out.println("KBps)");
    }
    private String input() throws IOException
    {
        BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
        return bf.readLine();
    }

    void logon() throws IPWorksEDIException
    {
        String buffer;
        try
        {
            System.out.print("User: " );
            buffer = input();
            ftp1.setUser( buffer );
            System.out.print("Password: ");
            buffer = input();
            ftp1.setPassword( buffer );
            ftp1.logon();
        }
        catch(IOException e)
        {
            System.out.println(e.getMessage());
        }
    }
    public void toggle_verbose()
    {
        verbose = 1 - verbose;
        System.out.print( "Verbose mode " );
        if (verbose == 1)
            System.out.println( "ON." );
        else
            System.out.println( "OFF." );
    }

    public static void main(String[] args)
    {
        new ftp(args);
    }
    public void dirList(FTPDirListEvent arg) {
        System.out.println(arg.dirEntry);
    }
    public void PITrail(FTPPITrailEvent arg) {
        if (verbose == 1)
            System.out.println(arg.message);
    }
    public void startTransfer() {
        transtime = System.currentTimeMillis();
    }
    public void transfer(FTPTransferEvent arg) {
        transbytes = arg.bytesTransferred;
    }
    public void endTransfer() {
        long endtime;
        endtime = System.currentTimeMillis();
        transtime = endtime - transtime;
    }
    public void error(FTPErrorEvent arg) {
        System.out.println("\nError "+arg.errorCode+": "+arg.description);
    }
    public void SSLServerAuthentication(FTPSSLServerAuthenticationEvent arg){
        arg.accept = true;
    }
}
class FTPEvents implements FTPEventListener{
		ftp instance;
    public FTPEvents(ftp instance){
        this.instance = instance;
    }
    public void connectionStatus(FTPConnectionStatusEvent arg) {
    }
    public void dirList(FTPDirListEvent arg) {
        instance.dirList(arg);
    }
    public void endTransfer(FTPEndTransferEvent arg) {
        instance.endTransfer();
    }
    public void error(FTPErrorEvent arg) {
        instance.error(arg);
    }
    public void PITrail(FTPPITrailEvent arg) {
    }
    public void startTransfer(FTPStartTransferEvent arg0) {
        instance.startTransfer();
    }
    public void transfer(FTPTransferEvent arg) {
        instance.transfer(arg);
    }
    public void SSLServerAuthentication(FTPSSLServerAuthenticationEvent arg){
        instance.SSLServerAuthentication(arg);
    }
    public void SSLStatus(FTPSSLStatusEvent arg){}
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



