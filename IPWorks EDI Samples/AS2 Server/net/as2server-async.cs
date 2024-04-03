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
using System.Net;
using System.Threading.Tasks;
using nsoftware.async.IPWorksEDI;

public class as2serverDemo
{
    private static As2receiver as2 = new As2receiver();

    public static async Task Main(string[] args)
    {
        try
        {
            HttpListener listener = new HttpListener();
            listener.Prefixes.Add("http://localhost:8080/");
            listener.Start();
            Console.WriteLine("Now listening for requests on " + 8080 + "...");

            while (true)
            {
                HttpListenerContext context = listener.GetContext();

                if (!string.Equals(context.Request.HttpMethod, "POST", StringComparison.OrdinalIgnoreCase))
                    return;

                Console.WriteLine("POST: " + context.Request.UserHostAddress);

                try
                {
                    Console.WriteLine("Reading request from: " + context.Request.UserHostAddress + "...");

                    // The first order of business is to parse the incoming headers
                    // This makes sure its an AS2 message, and determines the values of AS2From and AS2To.
                    // In an IIS application (like an ASP.NET application), you would normally do this by
                    // simply calling as2.ReadRequest(), but we are using an HttpListener for the HTTP server,
                    // so we have to do this manually

                    // Get the HTTP request headers
                    string inputHeaders = "";

                    for (int i = 0; i < context.Request.Headers.Count; i++)
                    {
                        inputHeaders += context.Request.Headers.GetKey(i) + ":" + context.Request.Headers.Get(i) + "\r\n";
                    }

                    as2.RequestHeadersString = inputHeaders;
                    await as2.SetRequestStream(context.Request.InputStream);

                    string myAS2Identifier = "AS2 Test Receiving Organization";

                    // After reading/parsing the request, check to make sure that this request is intended for you
                    if (!as2.AS2To.Equals(myAS2Identifier))
                        throw new Exception("The EDI message is meant for " + as2.AS2To + " not for us. [" + myAS2Identifier + "]");

                    // In a real AS2 application you would now check the values of
                    // AS2To and look up your trading partner's certificate in a database.
                    // This demo application uses a simple switch statement to load a
                    // partner certificate. Note that this certificate corresponds to the 
                    // as2sender.pfx used in the client demo. 
                    switch (as2.AS2From)
                    {
                        case "AS2 Test Sending Organization":
                            as2.SignerCert = new Certificate("../../../as2sender.cer");
                            break;
                        default:
                            //Alternatively, close the request
                            throw new Exception("Partner unknown: [" + as2.AS2From + "]");
                    }

                    // To sign receipts and/or decrypt incoming transmissions, you will need to
                    // set your certificate. (Note that by default, you do not need to explicitly
                    // tell the component to sign or decrypt; per AS2 standards message security is
                    // at the option of the client.

                    // For yourself, you will need a certificate with a private key. You can use a PFX
                    // file for this, or you can use a certificate directly from a Windows system store.
                    // Note that if you don't know the subject for your certificate, you can use the 
                    // CertMgr component to determine it.

                    // Note: this is the certificate that corresponds to the as2receiver.cer
                    // key that is included with the client.
                    as2.Certificate = new Certificate(CertStoreTypes.cstPFXFile, "../../../as2receiver.pfx", "test", "CN=AS2 Test Receiving Organization");

                    Console.WriteLine("\nProcessing request...");
                    await as2.ProcessRequest();
                    Console.WriteLine("The request has been successfully processed!\n");

                    // After processing the request and generating the MDN Receipt, you would normally 
                    // call as2.SendResponse() and Response.Close() if you were in an IIS application, but
                    // we have to manually create and send the response here since we're using the
                    // HTTPListener class for our HTTP server

                    // Get MDN Receipt headers and add them to the response as HTTP headers
                    string[] myHeaders = as2.MDNReceipt.Headers.Split("\r\n".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries);

                    for (int i = 0; i < myHeaders.Length; i++)
                    {
                        context.Response.Headers.Add(myHeaders[i]);
                    }

                    Console.WriteLine("Responding with MDN: ");
                    Console.WriteLine("======================================== MDN Headers ========================================");
                    Console.WriteLine(as2.MDNReceipt.Headers);
                    Console.WriteLine("=============================================================================================");
                    Console.WriteLine("============================= MDN Message (Human-Readable Part) =============================");
                    Console.WriteLine(as2.MDNReceipt.Message);
                    Console.WriteLine("=============================================================================================");
                    Console.WriteLine("====================================== Raw MDN Content ======================================");
                    Console.WriteLine(as2.MDNReceipt.Content);
                    Console.WriteLine("=============================================================================================");

                    // Add MDNReceipt contents to response contents
                    byte[] outputBuffer = as2.MDNReceipt.ContentB;
                    System.IO.Stream output = context.Response.OutputStream;
                    output.Write(outputBuffer, 0, outputBuffer.Length);
                    output.Close();

                    // Now that we have our response ready, we can send and close it
                    context.Response.Close();

                    Console.WriteLine("Successfully sent the MDN receipt\n\n");
                }
                catch (IPWorksEDIAs2receiverException e)
                {
                    Console.WriteLine("IPWorksEDIAs2receiverException: " + e.Message);
                }
                catch (Exception e)
                {
                    Console.WriteLine("Processing error: " + e.Message);
                }
            }
        }
        catch (HttpListenerException e)
        {
            Console.WriteLine("HttpListenerException: " + e.Message);
        }
        catch (Exception e)
        {
            Console.WriteLine("Exception: " + e.Message);
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