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
using System.IO;

class certmgrDemo
{
  private static Certmgr certmgr = new nsoftware.async.IPWorksEDI.Certmgr();
  private static string[] certificateList = null;

  static async Task Main(string[] args)
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

  private static async void SetStore(string storeType, string storeName, string password)
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
      certificateList = (await certmgr.ListStoreCertificates()).Split("\n");

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

  private static async void Create(string subject, int serialNumber, int certNumber)
  {
    if (certNumber < 0 || certNumber >= certificateList.Length)
    {
      await certmgr.CreateCertificate(subject, serialNumber);
    }
    else if (certmgr.CertStoreType != CertStoreTypes.cstPFXFile)
    {
      string[] chosenCert = certificateList[certNumber].Split("\t");
      certmgr.Cert = new Certificate(certmgr.CertStoreType, certmgr.CertStore, certmgr.CertStorePassword, chosenCert[0]);
      await certmgr.IssueCertificate(subject, serialNumber);
    }
    else
    {
      Console.WriteLine("Unsupported action.");
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