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

public class as2clientDemo
{
    private static AS2Sender as2 = new AS2Sender();

    private static void as2_OnSSLServerAuthentication(object? sender, AS2SenderSSLServerAuthenticationEventArgs e)
    {
        e.Accept = true;
    }

    public static void Main(string[] args)
    {
        if (args.Length < 4)
        {
            Console.WriteLine("Usage: as2client /FI AS2FromId /TI AS2ToId /U ReceiverURL /D EDIData /SC SignatureCert /SP SigningPass /RC RecipientCert /TM MDNTo\n");
            Console.WriteLine("  /FI  The AS2 From ID to use.");
            Console.WriteLine("  /TI  The AS2 To ID to use.");
            Console.WriteLine("  /U   The URL of the server that receives the data.");
            Console.WriteLine("  /D   The EDI data to send.");
            Console.WriteLine("  /SC  The sender's private key to use for signing. [Optional] (specify if you want to sign the outgoing data)");
            Console.WriteLine("  /SCP The password for the sender's private key. [Optional] (default value & password for the provided as2sender.pfx file is \"test\")");
            Console.WriteLine("  /RC  The recipient's certificate for encryption. [Optional] (specify if you want to encrypt the outgoing data)");
            Console.WriteLine("  /TM  Where to deliver the MDN (default: as2@nsoftware.com) [Optional] (specify if you want to receive an MDN)");
            Console.WriteLine("\nExample: as2client /FI \"AS2 Test Sending Organization\" /TI \"AS2 Test Receiving Organization\" /U \"http://localhost:1339/as2server.aspx\" /D" +
                " \"Sample EDI Data\" /TM \"as2@nsoftware.com\" /SC \"../../../as2sender.pfx\" /SCP \"test\" /RC \"../../../as2receiver.cer\"");
        }
        else
        {
            try
            {
                as2.OnSSLServerAuthentication += as2_OnSSLServerAuthentication;
                System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

                // The ASP.NET Development Server does not support chunked encoding. 
                // This demo disables chunked encoding for this reason. 
                // It can be enabled if you are not posting to the ASP.NET Development Server
                as2.Config("UseChunkedEncoding=false");

                // The first thing to do is to specify the necessary AS2 identifiers.
                as2.AS2From = myArgs["FI"];
                as2.AS2To = myArgs["TI"];

                // Set your partner's URL (HTTP or HTTPS) and the data to be sent. Note that if you are posting
                // to an HTTPS URL, you will likely need to set SSLAcceptServerCert.
                as2.URL = myArgs["U"];

                // At this point, you should set the certificates for yourself and your trading partner.
                // For yourself, you will need a certificate with a private key, in PKCS#12 (PFX) or JKS format.
                // If you don't know the subject for your certificate, you can use the CertMgr class to determine it.

                // Note that setting a signing certificate will instruct the class to sign the message. Leave these
                // properties blank to send an unsigned message.

                // Set the private key certificate for signing
                if (myArgs.ContainsKey("SC"))
                {
                    // The default password for the provided .pfx private key cert is 'test'
                    string password = (myArgs.ContainsKey("SCP")) ? myArgs["SCP"] : "test";
                    as2.SigningCert = new Certificate(CertStoreTypes.cstAuto, myArgs["SC"], password, "*");
                }

                // Similarly, setting the recipient's certificate will instruct the class to encrypt the message.
                // If you want to set a certificate, but don't want to encrypt the message, you can set
                // EncryptionAlgorithm to an empty string.

                // Set the recipient's certificate for encryption
                if (myArgs.ContainsKey("RC"))
                {
                    as2.RecipientCerts.Add(new Certificate(myArgs["RC"]));
                }

                // To request an MDN (Message Disposition Notification) based receipt, you should set the MDNTo
                // property by specifying the 'TM' parameter.
                if (myArgs.ContainsKey("TM"))
                {
                    // The actual value you feed to MDNTo is irrelevant,
                    // Most servers just check to see if something is specified at all
                    as2.MDNTo = "as2@nsoftware.com";

                    // For the purposes of this demo, we only support synchronous MDNs so we leave this blank
                    as2.MDNOptions = "";

                    // By default, the class will request that the receipt be delivered synchronously over the same
                    // HTTP connection. If you prefer to receive your receipt asynchronously, you should set
                    // MDNDeliveryOption, and provide additional processing for inbound asynchronous receipts.
                    // (See the .NET WinForms demo.)
                    // as2.MDNDeliveryOption = "https://localhost:59144/";
                }

                // If you set a log directory, the component will produce detailed log files
                as2.LogDirectory = "logs";

                // Specify EDI data to send
                as2.EDIData = new EDIData();
                as2.EDIData.EDIType = "application/edi-x12";
                as2.EDIData.Data = myArgs["D"];
                Console.WriteLine("Sending data to " + as2.URL + " ...");

                try
                {
                    // If the call to post() returns without throwing an exception, then the class was able to post
                    // the data and verify the response. In particular, if you requested a synchronous MDN,
                    // it will automatically be validated, and an exception will be thrown if there are any problems.

                    // If you requested an asynchronous MDN, you will need to save the values of MessageId,
                    // OriginalContentMIC, and MDNOptions, so they can be looked up based on the MessageId.
                    // Then, when you process the asynchronous MDN, you will need to load these values into
                    // the class to verify the MDN.
                    as2.Post();
                    bool mdnRequested = myArgs.ContainsKey("TM");
                    Console.WriteLine("Success! " + (mdnRequested ? "\nMDN Verified" : "\nNo MDN Requested"));

                    if (mdnRequested)
                    {
                        MDNReceipt receipt = as2.MDNReceipt;
                        Console.WriteLine("======================================== MDN Headers ========================================");
                        Console.WriteLine(receipt.Headers);
                        Console.WriteLine("=============================================================================================");
                        Console.WriteLine("============================= MDN Message (Human-Readable Part) =============================");
                        Console.WriteLine(receipt.Message);
                        Console.WriteLine("=============================================================================================");
                        Console.WriteLine("====================================== Raw MDN Content ======================================");
                        Console.WriteLine(receipt.Content);
                        Console.WriteLine("=============================================================================================");
                    }
                }
                catch (IPWorksEDIException e)
                {
                    Console.WriteLine("Sending failed.");
                    Console.WriteLine("Reason: " + e.Message);
                }
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
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
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