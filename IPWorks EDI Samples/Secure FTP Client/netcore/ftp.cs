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

class ftpDemo
{
  private static FTP ftp = new nsoftware.IPWorksEDI.FTP();

  private static void ftp_OnSSLServerAuthentication(object sender, FTPSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
  }

  private static void ftp_OnTransfer(object sender, FTPTransferEventArgs e)
  {
    Console.WriteLine(e.Text);
  }

  private static void ftp_OnDirList(object sender, FTPDirListEventArgs e)
  {
    Console.WriteLine(e.DirEntry);
  }

  static void Main(string[] args)
  {
    if (args.Length < 3)
    {
      Console.WriteLine("usage: ftp host user pass\n");
      Console.WriteLine("  host  the host to connect to");
      Console.WriteLine("  user  the username to use for authentication");
      Console.WriteLine("  pass  the password to use for authentication");
      Console.WriteLine("\nExample: ftp 192.168.1.2 myusername mypassword");
      Console.WriteLine("Press enter to continue.");
      Console.Read();
    }
    else
    {
      ftp.OnTransfer += ftp_OnTransfer;
      ftp.OnSSLServerAuthentication += ftp_OnSSLServerAuthentication;
      ftp.OnDirList += ftp_OnDirList;

      try
      {
        ftp.RemoteHost = args[0];
        ftp.User = args[1];
        ftp.Password = args[2];

        ftp.Logon();

        Console.WriteLine("Type \"?\" for a list of commands.");
        string command;
        string[] arguments;
        while (true)
        {
          ftp.RemoteFile = "";
          Console.Write("ftp> ");
          command = Console.ReadLine();
          arguments = command.Split();

          if (arguments[0] == "?" || arguments[0] == "help")
          {
            Console.WriteLine("?       cd      ls      pwd");
            Console.WriteLine("get     put     rm      passive");
            Console.WriteLine("mkdir   rmdir   exit");
          }
          else if (arguments[0] == "bye" || arguments[0] == "quit" || arguments[0] == "exit")
          {
            ftp.Logoff();
            break;
          }
          else if (arguments[0] == "cd")
          {
            if (arguments.Length > 1) ftp.ChangeRemotePath(arguments[1]);
          }
          else if (arguments[0] == "get")
          {
            if (arguments.Length > 1)
            {
              ftp.RemoteFile = arguments[1];
              ftp.LocalFile = arguments[1];
              ftp.Download();
              Console.WriteLine("File downloaded");
            }
          }
          else if (arguments[0] == "ls")
          {
            if (arguments.Length > 1)
            {
              string pathname = ftp.QueryRemotePath();
              ftp.ChangeRemotePath(arguments[1]);
              ftp.ListDirectoryLong();
              ftp.ChangeRemotePath(pathname);
            }
            else
            {
              ftp.ListDirectoryLong();
            }
          }
          else if (arguments[0] == "mkdir")
          {
            if (arguments.Length > 1) ftp.MakeDirectory(arguments[1]);
          }
          else if (arguments[0] == "mv")
          {
            if (arguments.Length > 2)
            {
              ftp.RemoteFile = arguments[1];
              ftp.RenameFile(arguments[2]);
            }
          }
          else if (arguments[0] == "passive")
          {
            if (arguments.Length > 1)
            {
              if ((arguments[1] == "on") && !ftp.Passive)
              {
                ftp.Passive = true;
                Console.WriteLine("Passive mode ON.");
              }
              else if ((arguments[1] == "off") && ftp.Passive)
              {
                ftp.Passive = false;
                Console.WriteLine("Passive mode OFF.");
              }
            }
          }
          else if (arguments[0] == "put")
          {
            if (arguments.Length > 2)
            {
              ftp.LocalFile = arguments[1];
              ftp.RemoteFile = arguments[2];
              ftp.Upload();
              Console.WriteLine("File uploaded");
            }
          }
          else if (arguments[0] == "pwd")
          {
            Console.WriteLine(ftp.QueryRemotePath());
          }
          else if (arguments[0] == "rm")
          {
            if (arguments.Length > 1) ftp.DeleteFile(arguments[1]);
          }
          else if (arguments[0] == "rmdir")
          {
            if (arguments.Length > 1) ftp.RemoveDirectory(arguments[1]);
          }
          else if (arguments[0] == "")
          {
            // Do nothing.
          }
          else
          {
            Console.WriteLine("Bad command / Not implemented in demo.");
          } // End of command checking.
        }
      }
      catch (Exception ex)
      {
        Console.WriteLine("Error: " + ex.Message);
      }
      Console.WriteLine("\npress <return> to continue...");
      Console.Read();
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