/*
 * IPWorks EDI 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using nsoftware.IPWorksEDI;

public class oftpclientDemo
{
    private static OFTPClient oftpclient = new OFTPClient();

    private static void oftpclient_OnSSLServerAuthentication(object? sender, OFTPClientSSLServerAuthenticationEventArgs e)
    {
        e.Accept = true;
    }

    private static void oftpclient_OnAcceptFile(object? sender, OFTPClientAcceptFileEventArgs e)
    {
        e.Overwrite = true;
        e.Accept = true;
    }

    private static void oftpclient_OnEndResponse(object? sender, OFTPClientEndResponseEventArgs e)
    {
        Console.WriteLine((e.Direction == 0) ? "Sent " : "Received " + "End Response for " + e.VirtualFileName + ".");
    }

    private static void oftpclient_OnPITrail(object? sender, OFTPClientPITrailEventArgs e)
    {
        // provides detailed logging of the interaction between the client and server
        Console.WriteLine((e.Direction == 0) ? "SERVER: " : "CLIENT: " + e.CommandDescription);
    }

    private static void oftpclient_OnEndTransfer(object? sender, OFTPClientEndTransferEventArgs e)
    {
        Console.WriteLine("Transfer has ended.");
    }

    private static void oftpclient_OnError(object? sender, OFTPClientErrorEventArgs e)
    {
        Console.WriteLine("ERROR! " + e.Description);
    }

    private static void oftpclient_OnStartTransfer(object? sender, OFTPClientStartTransferEventArgs e)
    {
        Console.WriteLine((e.Direction == 0 ? "Sending " : "Receiving ") + e.VirtualFileName);
    }

    private static void oftpclient_OnTransfer(object? sender, OFTPClientTransferEventArgs e)
    {
        Console.WriteLine("Starting transfer...");
    }

    public static void Main(string[] args)
    {
        if (args.Length < 2)
        {
            Console.WriteLine("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=");
            Console.WriteLine("usage: oftpclient /a address /p port /sssid serverssid /ssfid serversfid /spass serverpass /clssid clientssid " +
                "/clientsfid clientsfid /clpass clientpass /crt certificate /crtpass certpass /rcrt recipientcert /sec filesecurity /ssl /sigr /f file /d dir /v virtualfile");
            Console.WriteLine("  address        The address of the remot host.");
            Console.WriteLine("  port           The port on the remote host.");
            Console.WriteLine("  serverssid     The server's SSID code.");
            Console.WriteLine("  serversfid     The server's SFID code.");
            Console.WriteLine("  serverpass     The server's password");
            Console.WriteLine("  clientssid     The client's SSID code.");
            Console.WriteLine("  clientsfid     The client's SFID code.");
            Console.WriteLine("  clientpass     The client's password.");
            Console.WriteLine("  certificate    Specifies the certificate with private key for signing, decryption, and ssl.");
            Console.WriteLine("  certpass       Specifies the password for the certificates specified in cert & rcrt.");
            Console.WriteLine("  recipientcert  Specifies the public recipient certificate for encryption and signature validation.");
            Console.WriteLine("  filesecurity   Specifies the outgoing file security. Possible values are:");
            Console.WriteLine("    0 = None (default)");
            Console.WriteLine("    1 = Encrypted");
            Console.WriteLine("    2 = Signed");
            Console.WriteLine("    3 = Encrypted and Signed");
            Console.WriteLine("  ssl            Enables ssl when the client is started.");
            Console.WriteLine("  sigr           Requests a signed receipt when sending.");
            Console.WriteLine("  file           The path to the file to send (specify if you want to send a file).");
            Console.WriteLine("  dir            The directory to which the downloaded files are saved (specify if you want to receive files).");
            Console.WriteLine("  virtualfile    The name of the outgoing file. If not specified, the name of the local file is parsed and used as the virtual file name.");
            Console.WriteLine("\nExample (send + receive + ssl + signed + encrypted): \n\toftpclient /a localhost /p 3305 /sssid SERVERSSID /ssfid SERVERSFID /spass PASSWORD /clssid " +
                "CLIENTSSID /clsfid CLIENTSFID /clpass PASSWORD /crt \"./myfile.pfx\" /crtpass test /rcrt \"./recipient.cer\" /sec 3 /ssl /f testfile.txt /d \"./temp\" /v name.txt\n");
            Console.WriteLine("\nExample (plaintext send + receive): \n\toftpclient /a localhost /p 3305 /sssid SERVERSSID /ssfid SERVERSFID /spass PASSWORD /clssid " +
                "CLIENTSSID /clsfid CLIENTSFID /clpass PASSWORD /f testfile.txt /d \"./temp\" /v name.txt\n");
            Console.WriteLine("\nExample (plaintext send): \n\toftpclient /a localhost /p 3305 /sssid SERVERSSID /ssfid SERVERSFID /spass PASSWORD /clssid " +
                "CLIENTSSID /clsfid CLIENTSFID /clpass PASSWORD /f testfile.txt\n");
            Console.WriteLine("\nExample (plaintext receive): \n\toftpclient /a localhost /p 3305 /sssid SERVERSSID /ssfid SERVERSFID /spass PASSWORD /clssid " +
                "CLIENTSSID /clsfid CLIENTSFID /clpass PASSWORD /d \"./temp\" /v name.txt\n");
            Console.WriteLine("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=");
        }
        else
        {
            try
            {
                oftpclient.OnSSLServerAuthentication += oftpclient_OnSSLServerAuthentication;
                oftpclient.OnAcceptFile += oftpclient_OnAcceptFile;
                oftpclient.OnEndResponse += oftpclient_OnEndResponse;
                oftpclient.OnPITrail += oftpclient_OnPITrail;
                oftpclient.OnEndTransfer += oftpclient_OnEndTransfer;
                oftpclient.OnError += oftpclient_OnError;
                oftpclient.OnStartTransfer += oftpclient_OnStartTransfer;
                oftpclient.OnTransfer += oftpclient_OnTransfer;
                oftpclient.Overwrite = true;

                System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

                // set up client
                oftpclient.RemoteHost = myArgs["a"];
                oftpclient.RemotePort = Int32.Parse(myArgs["p"]);

                if (myArgs.ContainsKey("sssid")) oftpclient.ServerSSIDCode = myArgs["sssid"];
                if (myArgs.ContainsKey("ssfid")) oftpclient.ServerSFIDCode = myArgs["ssfid"];
                if (myArgs.ContainsKey("spass")) oftpclient.ServerPassword = myArgs["spass"];
                if (myArgs.ContainsKey("clssid")) oftpclient.ClientSSIDCode = myArgs["clssid"];
                if (myArgs.ContainsKey("clsfid")) oftpclient.ClientSFIDCode = myArgs["clsfid"];
                if (myArgs.ContainsKey("clpass")) oftpclient.ClientPassword = myArgs["clpass"];

                if (myArgs.ContainsKey("crt"))
                {
                    /* 
                     * The certificate with the private key is used to decrypt incoming messages and sign outgoing messages.
                     * In this demo, the same certificate will also be used to set SSLCert to ssl connections.
                     */
                    Certificate cert = new Certificate(CertStoreTypes.cstAuto, myArgs["crt"], myArgs["crtpass"], "*");
                    oftpclient.Certificate = cert;
                    oftpclient.SSLCert = cert;
                    Console.WriteLine("Certificate with subject: " + cert.Subject + " set.");
                }

                if (myArgs.ContainsKey("rcrt"))
                {
                    // In this demo the recipient certificate password is the same as the other certificate passwords.
                    Certificate cert = new Certificate(CertStoreTypes.cstAuto, myArgs["rcrt"], myArgs["crtpass"], "*");
                    oftpclient.RecipientCert = cert;
                }

                if (myArgs.ContainsKey("ssl"))
                {
                    oftpclient.UseSSL = true;

                    if (!myArgs.ContainsKey("crt"))
                        throw new Exception("When SSL is enabled, the cert argument must be specified.");
                }

                if (myArgs.ContainsKey("sec"))
                    oftpclient.VirtualFileSecurityLevel = (OFTPClientVirtualFileSecurityLevels)Int32.Parse(myArgs["sec"]);

                if (myArgs.ContainsKey("sigr"))
                    oftpclient.SignedReceipt = true;

                Console.WriteLine("Starting client...");

                // send file
                if (myArgs.ContainsKey("f"))
                {
                    /* 
                    * If the virtual file name is left empty, the local file's name will
                    * automatically be parsed and be used as the virtual file name
                    */
                    string virtualFileName = (myArgs.ContainsKey("v")) ? myArgs["v"] : "";
                    oftpclient.SendFile(myArgs["f"], virtualFileName);
                }

                // receive files
                if (myArgs.ContainsKey("d"))
                {
                    oftpclient.DownloadDirectory = myArgs["d"];
                    oftpclient.ReceiveFiles();
                }

                Console.WriteLine("Ending session.");
            }
            catch (IPWorksEDIException e)
            {
                Console.WriteLine("IPWorksEDIException: " + e.Message);
            }
            catch (Exception e)
            {
                Console.WriteLine("Exception: " + e.Message);
            }
        }
    }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add an key to the dictionary for each argument
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/" then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}