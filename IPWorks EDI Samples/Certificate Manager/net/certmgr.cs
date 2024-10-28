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
using System.IO;

class certmgrDemo
{
  private static CertMgr certmgr = new nsoftware.IPWorksEDI.CertMgr();
  private static string[] certificateList = null;

  static void Main(string[] args)
  {
    // Process user commands.
    Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
    Console.Write("certmgr> ");
    string command;
    string[] arguments;

    while (true)
    {
      command = Console.ReadLine();
      arguments = command.Split();

      if (arguments[0] == "?" || arguments[0] == "help")
      {
        Console.WriteLine("Commands: ");
        Console.WriteLine("  ?                                                      display the list of valid commands");
        Console.WriteLine("  help                                                   display the list of valid commands");
        Console.WriteLine("  store <user|machine|pfx> <filename|store> [password]   set and list the store");
        Console.WriteLine("    ex. store user MY");
        Console.WriteLine("    ex. store pfx test.pfx");
        Console.WriteLine("    ex. store pfx ..\\..\\..\\test6.pfx password");
        Console.WriteLine("  create <subject> <serial number> [certNumber]          create a certificate in the store. If no certificate \n" +
                          "                                                         is set, a self-signed certificate is created");
        Console.WriteLine("    ex. create CN=TestSubject 1111");
        Console.WriteLine("    ex. create CN=TestSubject 1111 2");
        Console.WriteLine("  quit                                                   exit the application");
      }
      else if (arguments[0] == "store")
      {
        if (arguments.Length == 3)
        {
          // No password specified.
          SetStore(arguments[1], arguments[2], "");
        }
        else if (arguments.Length == 4)
        {
          // Password specified.
          SetStore(arguments[1], arguments[2], arguments[3]);
        }
        else
        {
          Console.WriteLine("Please supply a valid number of arguments.");
        }
      }
      else if (arguments[0] == "create")
      {
        if (arguments.Length == 3)
        {
          // Create self-signed certificate.
          Create(arguments[1], int.Parse(arguments[2]), -1);
        }
        else if (arguments.Length == 4)
        {
          // Create certificate signed by specified certificate.
          Create(arguments[1], int.Parse(arguments[2]), int.Parse(arguments[3]));
        }
        else
        {
          Console.WriteLine("Please supply a valid number of arguments.");
        }
      }
      else if (arguments[0] == "")
      {
        // Do nothing.
      }
      else if (arguments[0] == "quit" || arguments[0] == "exit")
      {
        break;
      }
      else
      {
        Console.WriteLine("Invalid command.");
      } // End of command checking.

      Console.Write("certmgr> ");
    }
  }

  private static void SetStore(string storeType, string storeName, string password)
  {
    certmgr.CertStore = storeName;
    certmgr.CertStorePassword = password;

    switch (storeType.ToLower())
    {
      case "pfx":
        certmgr.CertStoreType = CertStoreTypes.cstPFXFile;
        break;
      case "user":
        certmgr.CertStoreType = CertStoreTypes.cstUser;
        break;
      case "machine":
        certmgr.CertStoreType = CertStoreTypes.cstMachine;
        break;
      default:
        throw new Exception("Please specify a valid store type.");
    }

    if (certmgr.CertStoreType == CertStoreTypes.cstUser || certmgr.CertStoreType == CertStoreTypes.cstMachine || File.Exists(storeName))
    {
      Console.WriteLine("Listing store certificates...");
      Console.WriteLine("Subject \t | CertIssuer \t | CertSerialNumber \t | HasPrivateKey");
      certificateList = (certmgr.ListStoreCertificates()).Split("\n");

      int i = 0;
      foreach (string line in certificateList)
      {
        if (line.Length > 0)
        {
          Console.WriteLine(i + ". " + line);
          i++;
        }
      }
    }
  }

  private static void Create(string subject, int serialNumber, int certNumber)
  {
    if (certNumber < 0 || certNumber >= certificateList.Length)
    {
      certmgr.CreateCertificate(subject, serialNumber);
    }
    else if (certmgr.CertStoreType != CertStoreTypes.cstPFXFile)
    {
      string[] chosenCert = certificateList[certNumber].Split("\t");
      certmgr.Cert = new Certificate(certmgr.CertStoreType, certmgr.CertStore, certmgr.CertStorePassword, chosenCert[0]);
      certmgr.IssueCertificate(subject, serialNumber);
    }
    else
    {
      Console.WriteLine("Unsupported action.");
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