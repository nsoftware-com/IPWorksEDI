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

﻿using System.Collections.Generic;
using System;
using nsoftware.IPWorksEDI;

class as4clientDemo
{
  private static AS4Client as4 = new nsoftware.IPWorksEDI.AS4Client();

  static void Main(string[] args)
  {
    try
    {
      System.Collections.Generic.Dictionary<string, string> myArgs = ConsoleDemo.ParseArgs(args);

      // Check args

      // All arguments are case-sensitive.

      foreach (string key in myArgs.Keys)
        Console.WriteLine(key + ": " + myArgs[key]);

      // Display help
      if (myArgs.ContainsKey("?"))
      {
        Console.WriteLine("Usage: as4 /Send|Receive /F FileToSend /D IncomingDirectory /P Profile /AR AgreementRef /AFR AS4FromRole /AFI AS4FromId /ATR AS4ToRole /ATI AS4ToId /U ServerURL /S /E \n");
        Console.WriteLine("  /Send|Receive       Whether to send or receive an AS4 message.");
        Console.WriteLine("  /P                  The AS4 profile to use. Must be either \"Standard\" or \"ENTSOG\".");
        Console.WriteLine("  /AR                 The agreement reference to use.");
        Console.WriteLine("  /AFR                The AS4 From role to use.");
        Console.WriteLine("  /AFI                The AS4 From ID to use.");
        Console.WriteLine("  /ATR                The AS4 To role to use.");
        Console.WriteLine("  /ATI                The AS4 To ID to use.");
        Console.WriteLine("  /U                  The URL of the server that either receives or sends files.");
        Console.WriteLine("  /S                  If provided the message will be signed. [Optional]");
        Console.WriteLine("  /E                  If provided the message will be encrypted. [Optional]");
        Console.WriteLine("  /D                  Directory used to receive incoming files.");
        Console.WriteLine("  /M                  Message channel ID.");
        Console.WriteLine("\nExample: /Send /F \"../../../test.xml\" /P \"Standard\" /AR \"http://agreements.company.com/agreement_01\" /AFR \"AS4 Test Sending Organization\" /AFI \"org:b2b:example:company:A\" /ATR \"AS4 Test Receiving Organization\" /ATI \"org:b2b:example:company:B\" /U \"http://localhost:1339/as4server.aspx\" /S /E\n");
        Console.WriteLine("\nExample: /Receive /D \"../../../Incoming\" /P \"Standard\" /M \"mpc_A\" /U \"http://localhost:1339/as4server.aspx\"\n");
        return;
      }

      if (myArgs.ContainsKey("Send"))
      {
        // The ASP.NET Development Server does not support chunked encoding. 
        // This demo disables chunked encoding for this reason. 
        // It can be enabled if you are not posting to the ASP.NET Development Server
        as4.Config("UseChunkedEncoding=false");

        as4.Profile = myArgs["P"] == "ENTSOG" ? AS4ClientProfiles.ebpfENTSOG : AS4ClientProfiles.ebpfStandard;
        as4.AgreementRef = myArgs["AR"];

        // The first thing to do is specify the necessary AS4 identifiers.
        EBPartyInfo as4from = new EBPartyInfo();
        as4from.Role = myArgs["AFR"];
        as4from.Id = myArgs["AFI"];
        as4.AS4From = as4from;

        EBPartyInfo as4to = new EBPartyInfo();
        as4to.Role = myArgs["ATR"];
        as4to.Id = myArgs["ATI"];
        as4.AS4To = as4to;

        if (myArgs.ContainsKey("S"))
        {
          as4.SigningCert = new Certificate(CertStoreTypes.cstAuto, "../../../as4client.pfx", "test", "*");
        }

        if (myArgs.ContainsKey("E"))
        {
          as4.Certificate = new Certificate("../../../as4server.cer");
        }

        as4.SignerCert = new Certificate("../../../as4server.cer");

        // By default, the component will request that the receipt be delivered
        // synchronously over the same HTTP connection. If you prefer to receive
        // your receipt asynchronously, you should set ReceiptReplyMode to Async, 
        // and provide additional processing for inbound asynchronous receipts.
        as4.ReceiptReplyMode = AS4ClientReceiptReplyModes.rrmSync;

        // If you set a log directory, the component will produce detailed log files.
        as4.LogDirectory = "logs";
        as4.URL = myArgs["U"];

        EBData data = new EBData();
        data.FileName = myArgs["F"];
        as4.EDIData.Add(data);

        try
        {
          as4.SendFiles();

          // If the call to SendFiles returns without throwing an exception, then the
          // component was able to post the data and verify the response. In particular,
          // if you requested a synchronous receipt, it will automatically be validated,
          // and an exception will be thrown if there are any problems.
          Console.WriteLine("Transmission was successful, and the receipt has been verified.");

          // If you requested an asynchronous receipt, Details about the original 
          // message must be stored so that the receipt can be correlated with the 
          // message and properly verified. The easiest way to do this is to set 
          // AsyncReceiptInfoDir before calling SendFiles. The component will 
          // automatically store the required information. See the VerifyReceipt 
          // method of AS4Server for details about verifying asynchronous receipts.
        }
        catch (nsoftware.IPWorksEDI.IPWorksEDIException ex)
        {
          Console.WriteLine("Transmission was unsuccessful: " + ex.Message);
        }
        finally
        {
          if (as4.Receipt != null)
          {
            Console.WriteLine("Message Id: " + as4.Receipt.RefToMessageId);
            Console.WriteLine(as4.Receipt.Content);
          }
        }
      }
      else if (myArgs.ContainsKey("Receive"))
      {
        as4.Profile = myArgs["P"] == "ENTSOG" ? AS4ClientProfiles.ebpfENTSOG : AS4ClientProfiles.ebpfStandard;

        as4.SigningCert = new Certificate(CertStoreTypes.cstAuto, "../../../as4client.pfx", "test", "*");
        as4.SignerCert = new Certificate("../../../as4server.cer");
        as4.Certificate = new Certificate(CertStoreTypes.cstAuto, "../../../as4client.pfx", "test", "*");

        as4.URL = myArgs["U"];

        as4.MPC = myArgs["M"];
        as4.IncomingDirectory = myArgs["D"];

        as4.Config("FilenameProperty=FileName");
        as4.ReceiveFiles();

        for (int i = 0; i < as4.EDIData.Count; ++i)
        {
          Console.WriteLine("Received File: " + as4.EDIData[i].FileName + "\r\n");
        }

        Console.WriteLine(as4.Receipt.RefToMessageId);
        Console.WriteLine("Receipt Content: \r\n" + as4.Receipt.Content + "\r\n");

        // Optionally save the receipt for later use
        // receiptContent = as4.Receipt.Content
        // receiptRefId = as4.Receipt.RefToMessageId

        // At this stage the receipt data is saved. Later when making another call to 
        // ReceiveFiles populate the Receipt property with this receipt data. 
        // When ReceiveFiles is called again, the receipt for the previous message 
        // will be included with the request. 
        // as4.Receipt = New EBReceipt(receiptRefId, receiptContent)
        // as4.ReceiveFiles() // This will now include the bundled receipt  
      }
      else
      {
        throw new Exception("Invalid operation. /Send or /Receive MUST be present.");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
    Console.ReadLine();
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