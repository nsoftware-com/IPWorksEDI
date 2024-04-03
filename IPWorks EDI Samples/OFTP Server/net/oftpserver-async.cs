/*
 * IPWorks EDI 2022 .NET Edition - Sample Project
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

using System.Collections.Generic;
﻿using System;
using System.IO;
using System.Threading.Tasks;
using nsoftware.async.IPWorksEDI;

public class oftpserverDemo
{
    private static Oftpserver oftpserver = new Oftpserver();
    private static string DownloadDir = "";
    private static string OutgoingDir = "";
    private static string ConnectionId = "";
    private static string ClientSSID = "";
    public static string RecipientCert = "";
    private static bool ReadyToSend = false;

    private static void oftpserver_OnSSLClientAuthentication(object? sender, OftpserverSSLClientAuthenticationEventArgs e)
    {
        e.Accept = true;
    }

    private static void oftpserver_OnAcceptConnection(object? sender, OftpserverAcceptConnectionEventArgs e)
    {
        ConnectionId = e.ConnectionId;
        ClientSSID = e.ClientSSIDCode;
        oftpserver.Connections[e.ConnectionId].DownloadDirectory = DownloadDir;

        if (!String.IsNullOrEmpty(RecipientCert))
        {
            oftpserver.Connections[e.ConnectionId].RecipientCertStoreType = CertStoreTypes.cstAuto;
            oftpserver.Connections[e.ConnectionId].RecipientCertStore = RecipientCert;
            oftpserver.Connections[e.ConnectionId].RecipientCertSubject = "*";
            Console.WriteLine("Recipient cert with subject " + oftpserver.Connections[e.ConnectionId].RecipientCertSubject + " set.");
        }

        e.Accept = true;
        Console.WriteLine("Connection accepted from " + oftpserver.Connections[e.ConnectionId].RemoteHost + ".");
    }

    private static void oftpserver_OnAcceptFile(object? sender, OftpserverAcceptFileEventArgs e)
    {
        e.Accept = true;
        e.Overwrite = true;
    }

    private static void oftpserver_OnStartTransfer(object? sender, OftpserverStartTransferEventArgs e)
    {
        if (e.Direction == 0)
        {
            Console.WriteLine("Started receiving file " + e.VirtualFileName + " to " + e.LocalFile + " ...");
        }
        else
        {
            Console.WriteLine("Sending file " + e.VirtualFileName + " ...");
        }
    }

    private static void oftpserver_OnEndTransfer(object? sender, OftpserverEndTransferEventArgs e)
    {
        Console.WriteLine(((e.Direction == 0) ? "Received " : "Sent ") + "file ");
    }

    private static void oftpserver_OnEndResponse(object? sender, OftpserverEndResponseEventArgs e)
    {
        Console.WriteLine(((e.Direction == 0) ? "Received " : "Sent ") + "End Response for " + e.VirtualFileName + ".");
    }

    private static void oftpserver_OnPITrail(object? sender, OftpserverPITrailEventArgs e)
    {
        Console.WriteLine(((e.Direction == 0) ? "CLIENT: " : "SERVER: ") + e.CommandDescription);
    }

    private static void oftpserver_OnReadyToSend(object? sender, OftpserverReadyToSendEventArgs e)
    {
        ReadyToSend = true;
    }

    private static void oftpserver_OnError(object? sender, OftpserverErrorEventArgs e)
    {
        Console.WriteLine("ERROR: " + e.Description);
    }

    public static async Task Main(string[] args)
    {
        if (args.Length < 5)
        {
            Console.WriteLine("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=");
            Console.WriteLine("Usage: oftpserver /ssid serverssid /sfid serversfid /pass serverpass /crt certificate /crtpass certpass /rcrt recipientcert" +
                "/sec filesecurity /ssl /sigr /ddir downloaddir /odir outgoingdir");
            Console.WriteLine("  serverssid     The server's SSID code.");
            Console.WriteLine("  serversfid     The server's SFID code.");
            Console.WriteLine("  serverpass     The server's password.");
            Console.WriteLine("  certificate    The certificate with private key for signing, decryption, and ssl.");
            Console.WriteLine("  certpass       The password for the certificate specified in cert");
            Console.WriteLine("  recipientcert  The public recipient certificate for encryption and signature validation.");
            Console.WriteLine("  filesecurity   Specifies the outgoing file security. Possible values are:");
            Console.WriteLine("    0 = None (default)");
            Console.WriteLine("    1 = Encrypted");
            Console.WriteLine("    2 = Signed");
            Console.WriteLine("    3 = Encrypted and Signed");
            Console.WriteLine("  ssl            Enables ssl when the client is started.");
            Console.WriteLine("  sigr           Requests a signed receipt when sending.");
            Console.WriteLine("  downloaddir    The directory where files received from the client are saved.");
            Console.WriteLine("  outgoingdir    The directory which holds files to be sent to the client.");
            Console.WriteLine("\nExample (plaintext): oftpserver /ssid SERVERSSID /sfid SERVERSFID /pass PASSWORD /ddir \"./download_dir/\" /odir \"./outgoing_dir/\"");
            Console.WriteLine("\nExample (ssl + signed + encrypted): oftpserver /ssid SERVERSSID /sfid SERVERSFID /pass PASSWORD /crt \"../../../oftpserver.pfx\" " +
                "/crtpass test /rcrt \"../../../recipient.cer\" /sec 3 /ssl /sigr /ddir \"./download_dir/\" /odir \"./outgoing_dir/\"");
            Console.WriteLine("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=");
        }
        else
        {
            try
            {
                oftpserver.OnSSLClientAuthentication += oftpserver_OnSSLClientAuthentication;
                oftpserver.OnAcceptConnection += oftpserver_OnAcceptConnection;
                oftpserver.OnAcceptFile += oftpserver_OnAcceptFile;
                oftpserver.OnStartTransfer += oftpserver_OnStartTransfer;
                oftpserver.OnEndTransfer += oftpserver_OnEndTransfer;
                oftpserver.OnEndResponse += oftpserver_OnEndResponse;
                oftpserver.OnPITrail += oftpserver_OnPITrail;
                oftpserver.OnReadyToSend += oftpserver_OnReadyToSend;
                oftpserver.OnError += oftpserver_OnError;

                // parse args
                Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

                if (myArgs.ContainsKey("ssid"))
                    oftpserver.ServerSSIDCode = myArgs["ssid"];

                if (myArgs.ContainsKey("sfid"))
                    oftpserver.ServerSFIDCode = myArgs["sfid"];

                if (myArgs.ContainsKey("pass"))
                    oftpserver.ServerPassword = myArgs["pass"];

                if (myArgs.ContainsKey("ddir"))
                    DownloadDir = myArgs["ddir"];
                else
                    throw new Exception("You must specify the download directory.");

                if (myArgs.ContainsKey("odir"))
                    OutgoingDir = myArgs["odir"];
                else
                    throw new Exception("You must specify the outgoing directory.");

                if (myArgs.ContainsKey("rcrt"))
                    RecipientCert = myArgs["rcrt"];

                if (myArgs.ContainsKey("crt"))
                {
                    /* 
                    * The certificate with the private key is used to decrypt incoming messages and sign outgoing messages.
                    * In this demo, the same certificate will also be used to set SSLCert to ssl connections.
                    */
                    Certificate cert = new Certificate(CertStoreTypes.cstAuto, myArgs["crt"], myArgs["crtpass"], "*");
                    oftpserver.Certificate = cert;
                    oftpserver.SSLCert = cert;
                    Console.WriteLine("Certificate with subject: " + cert.Subject + " set.");
                }

                if (myArgs.ContainsKey("ssl"))
                {
                    oftpserver.UseSSL = true;
                    oftpserver.LocalPort = 6619;

                    if (!myArgs.ContainsKey("crt"))
                        throw new Exception("When SSL is enabled, the cert argument must be specified.");
                }

                // start server
                Console.WriteLine("Starting server...");
                await oftpserver.StartListening();
                Console.WriteLine("Server listening on " + oftpserver.LocalHost + ":" + oftpserver.LocalPort);

                while (true && !ReadyToSend)
                {
                    await oftpserver.DoEvents();
                }

                // send files before closing the connection...

                if (myArgs.ContainsKey("sigr"))
                    oftpserver.Connections[ConnectionId].SignedReceipt = true;

                if (myArgs.ContainsKey("sec"))
                {
                    VirtualFileSecurityLevels level = (VirtualFileSecurityLevels)Int32.Parse(myArgs["sec"]);
                    oftpserver.Connections[ConnectionId].VirtualFileSecurityLevel = level;
                    Console.WriteLine("Setting outgoing file security level: " + level);
                }
                    
                if (Directory.Exists(OutgoingDir))
                {
                    // send all files found in outgoing dir
                    foreach (string filePath in Directory.GetFiles(OutgoingDir))
                    {
                        string fileName = Path.GetFileName(filePath);
                        await oftpserver.SendFile(ConnectionId, ClientSSID, filePath, fileName);
                    }
                }
                else
                {
                    throw new Exception("Outgoing directory does not exist.");
                }

                Console.WriteLine("Ending session.");

            }
            catch (IPWorksEDIOftpserverException e)
            {
                Console.WriteLine("IPWorksEDIOftpserverException: " + e.Message);
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
  public static Dictionary<string, string> ParseArgs(string[] args)
  {
    Dictionary<string, string> dict = new Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // If it starts with a "/" check the next argument.
      // If the next argument does NOT start with a "/" then this is paired, and the next argument is the value.
      // Otherwise, the next argument starts with a "/" and the current argument is a switch.

      // If it doesn't start with a "/" then it's not paired and we assume it's a standalone argument.

      if (args[i].StartsWith("/"))
      {
        // Either a paired argument or a switch.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Paired argument.
          dict.Add(args[i].TrimStart('/'), args[i + 1]);
          // Skip the value in the next iteration.
          i++;
        }
        else
        {
          // Switch, no value.
          dict.Add(args[i].TrimStart('/'), "");
        }
      }
      else
      {
        // Standalone argument. The argument is the value, use the index as a key.
        dict.Add(i.ToString(), args[i]);
      }
    }
    return dict;
  }

  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}