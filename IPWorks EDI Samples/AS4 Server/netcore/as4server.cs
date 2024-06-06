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
using System.Net;
using System.Text;
using nsoftware.IPWorksEDI;

public class as4serverDemo
{
    private static AS4Server as4 = new AS4Server();
    
    static void ParseAndAddHeaders(HttpListenerResponse response, string headersString)
    {
        // Separate headers by lines
        string[] lines = headersString.Split(new string[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries);

        foreach (var line in lines)
        {
            // names are whatever come before a ':', values are whatever comes after
            string headerName = line.Substring(0, line.IndexOf(":", StringComparison.Ordinal));
            string headerValue = line.Substring(line.IndexOf(":", StringComparison.Ordinal) + 1).Trim();

            if (headerName.Equals("Content-Type"))
            {
                response.ContentType = headerValue;
                continue;
            }

            response.Headers.Add(headerName, headerValue);
        }
    }

    public static void Main(string[] args)
    {
        try
        {
            HttpListener listener = new HttpListener();
            listener.Prefixes.Add("http://localhost:8080/");
            Console.WriteLine("Starting server...");
            listener.Start();
            Console.WriteLine("Now listening for HTTP requests on \"" + string.Join("", listener.Prefixes) + "\" ...");

            // Since we use an HttpListener to send HTTP responses, we cannot
            // depend on the AS4Server.SendResponse method to do this for us. 
            // By setting the ResponseToString config setting to true, the 
            // SendResponse setting will instead allow us to store the response
            // contents to memory so we can manually sent it via the HttpListener
            // later
            as4.Config("ResponseToString=true");

            while (listener.IsListening)
            {
                HttpListenerContext context = listener.GetContext();

                if (!string.Equals(context.Request.HttpMethod, "POST", StringComparison.OrdinalIgnoreCase))
                    return;

                Console.WriteLine("POST: \"" + context.Request.UserHostAddress + "\"");

                try
                {
                    // Get request headers
                    string inputHeaders = "";

                    for (int i = 0; i < context.Request.Headers.Count; i++)
                    {
                        inputHeaders += context.Request.Headers.GetKey(i) + ":" + context.Request.Headers.Get(i) + "\r\n";
                    }

                    // The first step is to call ReadRequest. This will parse out relevant information 
                    // from the incoming message including party information and more.
                    // Since we are working within an HttpListener context in a non IIS application like ASP.NET,
                    // we must manually feed the server component the HTTP request data before calling ReadRequest

                    // Set the input headers
                    as4.RequestHeadersString = inputHeaders;

                    // Set the request contents
                    as4.SetRequestStream(context.Request.InputStream);
                    
                    // Parse the request
                    as4.ReadRequest();

                    // If MPC is not empty then this is a Pull request
                    if (string.IsNullOrEmpty(as4.AgreementRef) && !string.IsNullOrEmpty(as4.MPC))
                    {
                        switch (as4.MPC)
                        {
                            case "mpc_A":
                                as4.AgreementRef = "http://agreements.company.com/agreement_01";
                                as4.AS4From.Id = "org:b2b:example:company:B";
                                as4.AS4From.Role = "AS4 Test Sending Organization";
                            
                                as4.AS4To.Id = "org:b2b:example:company:A";
                                as4.AS4To.Role = "AS4 Test Receiving Organization";

                                as4.ReceiptReplyMode = AS4ServerReceiptReplyModes.rrmAsync;

                                //  Private certificate used to sign the message and files
                                as4.SigningCert = new Certificate(CertStoreTypes.cstPFXFile, "../../../as4server.pfx", "test", "CN=AS4 Server Certificate");

                                //  Partner's public certificate. Used to encrypt files
                                as4.RecipientCerts.Add(new Certificate("../../../as4client.cer"));

                                //  To preserve the filename of the first XML part when using the Standard profile set the FilenameProperty setting.
                                //  See the documentation for FilenameProperty for details.
                                as4.Config("FilenameProperty=FileName");

                                EBData data = new EBData();
                                data.Data = "<example-document><content>This is just a very simple XML document to show transport of XML payloads in the SOAP body</content></example-document>";
                                data.EDIType = "text/xml";
                                data.Name = "test.xml";
                                as4.EDIData.Add(data);

                                data = new EBData();
                                data.Data = "<example-document><content>This is a second simply XML document for demonstration purposes</content></example-document>";
                                data.EDIType = "text/xml";
                                data.Name = "test2.xml";
                                as4.EDIData.Add(data);

                                Console.WriteLine("Sending EDI data to " + context.Request.UserHostAddress);
                                as4.SendResponse();
                                break;
                            default:
                                throw new Exception("MPC unknown: [" + as4.MPC + "]");
                        }
                    }
                    else
                    {
                        // The client is sending files. Process the incoming files and send a signed receipt.
                        string myAS4Identifier = "org:b2b:example:company:B";
                        //  Check to make sure that this request is intended for you
                        if (!as4.AS4To.Id.Equals(myAS4Identifier))
                            throw new Exception("The EDI message is meant for " + as4.AS4To.Id + " not for us. [" + myAS4Identifier + "]");

                        //  In a real AS4 application you would now check the values of
                        //  AS4To and look up your trading partner's certificate in a database.
                        //  This demo application uses a simple switch statement to load a
                        //  partner certificate. Note that this certificate corresponds to the 
                        //  as4client.pfx used in the client demo.
                        switch (as4.AS4From.Id)
                        {
                            case "org:b2b:example:company:A":
                                as4.SignerCert = new Certificate("../../../as4client.cer");
                                break;
                            default:
                                //  Alternatively, close the request
                                throw new Exception("Partner unknown: [" + as4.AS4From.Id + "]");
                        }

                        //  To sign receipts and/or decrypt incoming transmissions, you will need to
                        //  set your certificate. Note that below the certificate used for signing
                        //  the response and for decryption is the same, but technically different certs
                        //  could be used here.

                        //  Private certificate, used to sign the receipt.
                        as4.SigningCert = new Certificate(CertStoreTypes.cstPFXFile, "../../../as4server.pfx", "test", "CN=AS4 Server Certificate");

                        //  Private certificate, used to decrypt the incoming file. Note: this is the certificate that corresponds to the as4server.cer
                        as4.Certificate = new Certificate(CertStoreTypes.cstPFXFile, "../../../as4server.pfx", "test", "CN=AS4 Server Certificate");

                        //  IncomingDirectory can be set to automatically store the incoming files.
                        //  When ParseRequest is called, EDIData will also be populated with the received
                        //  file information.
                        as4.IncomingDirectory = "../../../Incoming";

                        Console.WriteLine("Receiving EDI data from " + context.Request.UserHostAddress);

                        //  This application will attempt to process the request and report any error
                        //  conditions that are encountered to the sender in the receipt. You can also 
                        //  call ProcessRequest to process the message and create the receipt in one step
                        as4.ParseRequest();

                        as4.ReceiptReplyMode = AS4ServerReceiptReplyModes.rrmSync;
                        as4.SendResponse();
                    }

                    // If your application works within an IIS context (like in ASP.NET),
                    // calling SendResponse() is enough, but to get this to send the response
                    // through the HTTPListener, we have to manually send the response ourselves.
                
                    Console.WriteLine("Sending response to \"" + context.Request.UserHostAddress + "\"...");

                    // Get response contents generated from calling SendResponse()
                    string responseHeaders = as4.Config("ResponseHeaders");
                    string responseBody = as4.Config("ResponseBody");

                    // ResponseHeaders returns one singular string with all of the headers,
                    // so we have to manually parse the string and split them up into (name, value)
                    // pairs to add them to the response headers.
                    ParseAndAddHeaders(context.Response, responseHeaders);

                    // Add the response body to the output stream
                    byte[] bodyBytes = Encoding.Default.GetBytes(responseBody);
                    context.Response.OutputStream.Write(bodyBytes, 0, bodyBytes.Length);

                    // Actually send the response
                    context.Response.Close();
                    
                    Console.WriteLine("Response sent!");
                }
                catch (IPWorksEDIException e)
                {
                    Console.WriteLine("IPWorksEDIException: " + e.Message);
                    context.Response.StatusCode = 400;
                    context.Response.OutputStream.Write(Encoding.Default.GetBytes(e.Message));
                    context.Response.Close();
                }
                finally
                {
                    listener.Close();
                }
            }
        }
        catch (IPWorksEDIException e)
        {
            Console.WriteLine("IPWorksEDIException: " + e.Message);
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