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

class as1senderDemo : ConsoleDemo
{
  private static AS1Sender as1sender = new nsoftware.IPWorksEDI.AS1Sender();
  private static AS1Receiver as1receiver = new nsoftware.IPWorksEDI.AS1Receiver();

  static void Main(string[] args)
  {
    try
    {
      string ediDataPath = "";
      string originalContentMIC = ""; // Used to verify a sent message. 
      string mdnOptions = "";

      Console.WriteLine("Type \"?\" for a list of commands.");
      string rawline;
      string command;
      string[] arguments;
      while (true)
      {
        Console.Write("as1> ");
        rawline = Console.ReadLine();
        if (rawline.IndexOf(" ") > 0)
        {
          command = rawline.Substring(0, rawline.IndexOf(" "));
          arguments = rawline.Replace(command, "").Split(" ");
        }
        else
        {
          command = rawline;
          arguments = new string[0];
        }

        if (command == "send")
        {
          as1sender = new nsoftware.IPWorksEDI.AS1Sender();

          Console.WriteLine("Complete the prompts to send an AS1 message.");
          Console.WriteLine("MDNOptions: " + as1sender.MDNOptions);

          as1sender.MailServer = Prompt("SMTP server", "localhost");
          //as1sender.Config("SMTPPort=" + mailPort);
          as1sender.From = Prompt("From", "");
          as1sender.SendTo = Prompt("To", "");
          as1sender.Subject = Prompt("Subject", "Test AS1 Message " + DateTime.Now.ToString("dd-MM-yyyy"));
          as1sender.EDIData = new EDIData(Prompt("File to send", "../../../x12.txt"), "application/edi-x12");
          as1sender.LogDirectory = Prompt("Log directory", "../../../logs");

          as1sender.SigningCert = new Certificate(CertStoreTypes.cstPFXFile, "../../../testcert.pfx", "password", "*");
          as1sender.RecipientCerts.Add(new Certificate("../../../testcert.cer"));

          as1sender.Send();

          originalContentMIC = as1sender.OriginalContentMIC;
          mdnOptions = as1sender.MDNOptions;

          // Save these properties of the original posting, so that the receipt can
          // be checked. In a real AS1 application you would need to save these
          // externally (for example, in a database)
          Console.WriteLine("Send is finished.");
          Console.WriteLine("OriginalContentMIC: " + as1sender.OriginalContentMIC);
          Console.WriteLine("MessageId: " + as1sender.MessageId);
          Console.WriteLine("MdnOptions: " + as1sender.MDNOptions);
        }
        else if (command == "recv")
        {
          as1receiver = new nsoftware.IPWorksEDI.AS1Receiver();

          Console.WriteLine("Complete the prompts to receive an AS1 message.");

          as1receiver.MailServer = Prompt("POP server", "localhost");
          //as1receiver.Config("POPPort=" + mailPort);
          as1receiver.User = Prompt("User", ""); ;
          as1receiver.Password = Prompt("Password", "");
          as1receiver.LogDirectory = Prompt("Log directory", "../../../logs");

          as1receiver.Connect();

          // Assume that the last message in the mailbox is the AS1 message. In a real
          // AS1 server you would use the other MailMessage properties to navigate the inbox
          // and look for AS1 messages. The component can in fact be used to write a fairly
          // complete POP client.

          as1receiver.MailMessageNumber = as1receiver.MailMessageCount;
          as1receiver.ReadRequest();
          as1receiver.Disconnect();

          // Set any required certificates here.
          as1receiver.Certificate = new Certificate(CertStoreTypes.cstPFXFile, "../../../testcert.pfx", "password", "*");
          as1receiver.SignerCert = new Certificate("../../../testcert.cer");

          // Process the request and generate an MDN receipt.
          as1receiver.ProcessRequest();

          // At this point you can read the SignatureType, EncryptionType, etc. properties to determine
          // what security was applied to the message, and you can read the EDIData and EDIType files to
          // get at the data.

          // Unless you don't like the data or security parameters, you should send a receipt in response.
          // Note that if your trading partner didn't request a receipt this will default to a noop.
          as1receiver.SendResponse();
          Console.WriteLine("Receive is finished.");
        }
        else if(command == "vrfy")
        {
          //as1sender = new nsoftware.IPWorksEDI.AS1Sender();

          Console.WriteLine("Complete the prompts to verify the MDN receipt of an AS1 message.");

          as1sender.MailServer = Prompt("POP server", "localhost");
          as1sender.User = Prompt("User", "");
          as1sender.Password = Prompt("Password", "");
          as1sender.LogDirectory = Prompt("Log directory", "../../../logs");

          // As above, assume the receipt is the most recent message in the mailbox.
          try
          {
            as1sender.Connect();

            as1sender.MailMessageNumber = as1sender.MailMessageCount;
            as1sender.ReadReceipt();

            Console.WriteLine("Receipt read.");

            // After reading the receipt, the component will determine the sender
            // of the receipt, and the Message-Id for which the receipt is being sent.
            // This will allow you to look up the OriginalContentMIC and MDNOptions
            // from your database.
            Console.WriteLine("The receipt is from " + as1sender.SendTo + "\r\n");
            Console.WriteLine("Message-Id: " + as1sender.MessageId + "\r\n");

            as1sender.OriginalContentMIC = Prompt("MIC of a sent message", originalContentMIC);
            as1sender.MDNOptions = Prompt("MDN options of a sent message", mdnOptions);
            as1sender.VerifyReceipt();

            Console.WriteLine("Receipt verified.");
            Console.WriteLine("Publickey: \r\n" + as1sender.RecipientCerts[0].PublicKey);
          }
          catch (Exception ex)
          {
            Console.WriteLine("Error reading or verifying receipt: " + Environment.NewLine + ex.Message);
          }
          finally
          {
            as1sender.Disconnect();
          }
        }
        else if (command == "exit")
        {
          as1receiver.Disconnect();
          as1sender.Disconnect();
          break;
        }
        else
        {
          Console.WriteLine("?  send  recv  vrfy  exit");
        } // end of command checking
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
      Console.ReadLine();
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