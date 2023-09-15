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
using System.Threading.Tasks;
using nsoftware.async.IPWorksEDI;

class imapDemo
{
  private static Imap imap = new nsoftware.async.IPWorksEDI.Imap();
  private static int lines = 0;

  private static void imap_OnSSLServerAuthentication(object sender, ImapSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void imap_OnMailboxList(object sender, ImapMailboxListEventArgs e)
  {
    Console.WriteLine(e.Mailbox);
    lines++;
    if (lines == 22)
    {
      Console.Write("Press enter to continue...");
      Console.ReadLine();
      lines = 0;
    }
  }

  private static void imap_OnMessageInfo(object sender, ImapMessageInfoEventArgs e)
  {
    Console.Write(e.MessageId + "  ");
    Console.Write(e.Subject + "  ");
    Console.Write(e.MessageDate + "  ");
    Console.WriteLine(e.From);
    lines++;
    if (lines == 22)
    {
      Console.Write("Press enter to continue...");
      Console.ReadLine();
      lines = 0;
    }
  }

  private static void imap_OnTransfer(object sender, ImapTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
    lines++;
    if (lines == 22)
    {
      Console.Write("Press enter to continue...");
      Console.ReadLine();
      lines = 0;
    }
  }

  static async Task Main(string[] args)
  {
    if (args.Length < 3)
    {
      Console.WriteLine("usage: imap server username password");
      Console.WriteLine("  server    the name or address of the mail server (IMAP server)");
      Console.WriteLine("  username  the user name used to authenticate to the MailServer");
      Console.WriteLine("  password  the password used to authenticate to the MailServer");
      Console.WriteLine("\nExample: imap 127.0.0.1 username password");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {
      try
      {
        imap.OnSSLServerAuthentication += imap_OnSSLServerAuthentication;
        imap.OnMailboxList += imap_OnMailboxList;
        imap.OnMessageInfo += imap_OnMessageInfo;
        imap.OnTransfer += imap_OnTransfer;

        imap.MailServer = args[args.Length - 3];
        imap.User = args[args.Length - 2];
        imap.Password = args[args.Length - 1];

        // Uncomment these lines if the IMAP server requires SSL.
        //imap.SSLEnabled = true;
        //imap.SSLStartMode = ImapSSLStartModes.sslAutomatic;
        //imap.MailPort = 993;

        Console.WriteLine("Connecting.");
        await imap.Connect();
        DisplayMenu();

        string command;
        string[] argument;
        int msgnum = 1;
        do
        {
          Console.Write("imap> ");
          command = Console.ReadLine();
          argument = command.Split();
          if (argument.Length == 0 || string.IsNullOrEmpty(argument[0]))
            continue;
          switch (argument[0][0])
          {
            case 's':
              try
              {
                if (argument.Length < 2)
                {
                  Console.WriteLine("Must provide a mailbox to select.");
                }
                imap.Mailbox = argument[1];
                await imap.SelectMailbox();
              }
              catch (Exception ex)
              {
                Console.WriteLine(ex.Message);
              }
              break;
            case 'h':
              try
              {
                if (imap.MessageCount > 0)
                {
                  if (imap.MessageSet == "") imap.MessageSet = "1:" + imap.MessageCount;
                  await imap.FetchMessageInfo();
                }
                else
                {
                  Console.WriteLine("No messages in this mailbox.");
                }
              }
              catch (Exception ex)
              {
                Console.WriteLine(ex.Message);
              }
              break;
            case 'l':
              try
              {
                imap.Mailbox = argument.Length < 2 ? "*" : imap.Mailbox = argument[1];
                await imap.ListMailboxes();
              }
              catch (Exception ex)
              {
                Console.WriteLine(ex.Message);
              }
              break;
            case 'n':
              try
              {
                msgnum++;
                imap.MessageSet = msgnum.ToString();
                await imap.FetchMessageText();
              }
              catch (Exception ex)
              {
                Console.WriteLine(ex.Message);
              }
              break;
            case 'q':
              await imap.Disconnect();
              return;
            case 'v':
              try
              {
                if (argument.Length < 2)
                {
                  Console.WriteLine("Message number required.");
                  continue;
                }
                msgnum = int.Parse(argument[1]);
                imap.MessageSet = argument[1];
                await imap.FetchMessageText();
              }
              catch (Exception ex)
              {
                Console.WriteLine(ex.Message);
              }
              break;
            case '?':
              DisplayMenu();
              break;
            default: // allow user to enter only the number of the message they
                     // want to view
              try
              {
                msgnum = int.Parse(command);
                imap.MessageSet = command;
                await imap.FetchMessageText();
              }
              catch (FormatException e)
              {
                Console.WriteLine("Bad command / Not implemented in demo.");
              }
              break;
          }
        } while (true);
      }
      catch (Exception ex)
      {
        Console.WriteLine(ex.Message);
      }
      Console.WriteLine("Press any key to exit...");
      Console.ReadKey();
    }
  }

  private static void DisplayMenu()
  {
    Console.WriteLine("IMAP Commands");
    Console.WriteLine("  l                   list mailboxes");
    Console.WriteLine("  s <mailbox>         select mailbox");
    Console.WriteLine("  v <message number>  view the content of selected message");
    Console.WriteLine("  n                   goto and view next message");
    Console.WriteLine("  h                   print out active message headers");
    Console.WriteLine("  ?                   display options");
    Console.WriteLine("  q                   quit");
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