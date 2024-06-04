/*
 * IPWorks EDI 2022 C++ Edition - Sample Project
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
 */


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <time.h>
#include <fstream>
#include <strsafe.h>
#include "../../include/ipworksedi.h"

using namespace std;

#define SERVER_ID "SERVERSSID"
#define SERVER_PASS "PASSWORD"


char input[MAX_PATH + 1];
char* recipientCert;
bool readyToSend = false;
int clientConnectionId = -1;
bool signedReceipt = false;
int fileSecurityLevel = -1;
char* clientSSIDCode;
char* DownloadDir;
char* OutgoingDir;

char* GetArg(const char* option,int argc, char** argv) {
    for (int x = 0; x < argc; x++) if (!strcmp(argv[x], option) && x != argc - 1) return argv[x + 1];
    return (char*)"";
  }

bool ArgExists(const char* option, int argc, char** argv) {
    for (int x = 0; x < argc; x++) if (!strcmp(argv[x], option)) return true;
    return false;
  }

void Log(char* message) {

    time_t rawtime;
    time(&rawtime);
    char buffer[MAX_PATH];

    struct tm* timeinfo1;
    timeinfo1 = gmtime(&rawtime);
    strftime(buffer, MAX_PATH, "%H:%M:%S", timeinfo1);

    printf("[%s] %s\n", buffer,message);
}

const char* prompt(const char* prompt, const char* defaultVal) {
    printf("%s [%s]: ", prompt, defaultVal);
    fgets(input, MAX_PATH, stdin);
    input[strlen(input) - 1] = '\0';
    if (strlen(input) == 0) {
        strncpy(input, defaultVal, MAX_PATH);
        input[MAX_PATH] = '\0';
    }
    return input;
}


class MyOFTPServer : public OFTPServer {
public:
    MyOFTPServer() {}

    virtual int FireAcceptConnection(OFTPServerAcceptConnectionEventParams* e) {
        char logmsg[MAX_PATH];
        clientConnectionId = e->ConnectionId;
        clientSSIDCode = strdup(e->ClientSSIDCode);

        this->SetOFTPConnectionDownloadDirectory(e->ConnectionId, DownloadDir);
        
        if (strlen(recipientCert)) {
            //The recipient cert is a public certificate used to encrypt outgoing messages and verify the signature of incoming messages
            this->SetOFTPConnectionRecipientCertStoreType(e->ConnectionId,CST_AUTO);
            this->SetOFTPConnectionRecipientCertStore(e->ConnectionId, recipientCert,strlen(recipientCert));
            int ret_code = this->SetOFTPConnectionRecipientCertSubject(e->ConnectionId, "*");
            if (ret_code) {
                printf("Error [%d] : %s\n", this->GetLastErrorCode(), this->GetLastError());
                return ret_code;
            }
            sprintf(logmsg, "Recipient cert with subject \"%s\" set.", this->GetOFTPConnectionRecipientCertSubject(e->ConnectionId));
            Log(logmsg);
        }

        e->Accept = true;
        sprintf(logmsg, "Connection accepted from %s.", this->GetOFTPConnectionRemoteHost(e->ConnectionId));
        Log(logmsg);

        return 0;
    }
    virtual int FireAcceptFile(OFTPServerAcceptFileEventParams* e) {
        e->Overwrite = true;
        return 0;
    }
    virtual int FireStartTransfer(OFTPServerStartTransferEventParams* e) {

        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "Started receiving file \"%s\" to \"%s\" ...", e->VirtualFileName, e->LocalFile);
            
        }
        else {
            sprintf(logmsg, "Sending file \"%s\" ...", e->VirtualFileName);
        }

        Log(logmsg);
        return 0;
    }
    virtual int FireEndTransfer(OFTPServerEndTransferEventParams* e) {
        char logmsg[MAX_PATH];

        if (e->Direction == 0) {
            sprintf(logmsg, "Received file \"%s\" to \"%s\".", e->VirtualFileName, e->LocalFile);
        }
        else {
            sprintf(logmsg, "Sent file \"%s\".", e->VirtualFileName);
        }

        Log(logmsg);
        return 0;
    }

    virtual int FireEndResponse(OFTPServerEndResponseEventParams *e) {
        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "Received End Response for %s.", e->VirtualFileName);
        }
        else {
            sprintf(logmsg, "Sent End Response for %s.", e->VirtualFileName);
        }
        Log(logmsg);
        return 0;
    }

    virtual int FirePITrail(OFTPServerPITrailEventParams* e) {
        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "CLIENT: %s", e->CommandDescription);
        }
        else {
            sprintf(logmsg, "SERVER: %s", e->CommandDescription);
        }

        // Uncomment to enable protocol level logging
        //Log(logmsg);
        return 0;
    }

    virtual int FireReadyToSend(OFTPServerReadyToSendEventParams* e) {
        readyToSend = true;
        return 0;
    }

    virtual int FireError(OFTPServerErrorEventParams* e) {
        Log(strdup(e->Description));
        return 0;
    }
};

int SendFilesToClient(MyOFTPServer& server) {

    int ret_code = 0; 

    // Set configuration options.
    server.SetOFTPConnectionSignedReceipt(clientConnectionId, signedReceipt);

    if (fileSecurityLevel >= 0) {
        // 0 = none
        // 1 = encrypted
        // 2 = signed
        // 3 = encrypted and signed
        server.SetOFTPConnectionVirtualFileSecurityLevel(clientConnectionId, fileSecurityLevel);

        char logmsg[MAX_PATH];
        sprintf(logmsg, "Setting outgoing file security level: %d", fileSecurityLevel);
        Log(logmsg);
    }

    //List files in the outgoing folder
    WIN32_FIND_DATA ffd;
    HANDLE hFind = INVALID_HANDLE_VALUE;
    TCHAR szDir[MAX_PATH];
    char fileMask[MAX_PATH];
    char outgoingFile[MAX_PATH];
    
    sprintf(fileMask, "%s\\*", OutgoingDir);
    StringCchCopy(szDir, MAX_PATH, fileMask);
    hFind = FindFirstFile(szDir, &ffd);
    if (hFind != INVALID_HANDLE_VALUE) {
        do {
            if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
                continue;

            sprintf(outgoingFile, "%s\\%s", OutgoingDir, ffd.cFileName);
            
            ret_code = server.SendFile(clientConnectionId, clientSSIDCode, outgoingFile, ffd.cFileName);
            if (ret_code) return ret_code;

        } while (FindNextFile(hFind, &ffd) != 0);
    }

    //Close the connection. This is the last operation that would occur before the connection is complete.
    Log("Ending session.");
    ret_code = server.Logoff(clientConnectionId);
    return ret_code;
};

void CreateTestFilesAndDirs() {
    //Create working directories

    //Create outgoing test files
    for (int i = 0; i < 3; i++) {
        char testFileName[MAX_PATH];
        sprintf(testFileName, "%s\\test%d.txt", OutgoingDir, i);
        ofstream testFile(testFileName);
        testFile << "This is some test data";
        testFile.close();
    }
}

void PrintUsage() {
    printf("************************************************************\n");
    printf("The command line arguments below control the operation of the server.");
    printf("By default only the --download-dir parameter is required and the server will operate in plaintext mode.\n");
    printf("\n");
    printf("usage: oftpserver.exe [--incoming-dir] [--outgoing-dir] [--cert] [--certpass] [--recipient-cert] [--file-security] [--signed-recipt] [--ssl]\n");
    printf("\n");
    printf("Parameter details:\n");
    printf("\t--download-dir C:\\oftp\\incoming\n");
    printf("\t\tThe directory where files received from the client are saved.\n");
    printf("\t--outgoing-dir C:\\oftp\\outgoing\n");
    printf("\t\tThe directory which holds files to be sent to the client.\n");
    printf("\t--cert C:\\somewhere\\myfile.pfx\n");
    printf("\t\tSpecifies the certificate with private key for signing, decryption, and ssl.\n");
    printf("\t--certpass mypassword\n");
    printf("\t\tSpecifies the password for the certificate specified in --cert\n");
    printf("\t--recipient-cert C:\\somewhere\\recpiient.cer\n");
    printf("\t\tSpecifies the public recipient certificate for encryption and signature validation.\n");
    printf("\t--file-security [0|1|2|3]\n");
    printf("\t\tSpecifies the outgoing file security. Possible values are:\n");
    printf("\t\t0 = None\n");
    printf("\t\t1 = Encrypted\n");
    printf("\t\t2 = Signed\n");
    printf("\t\t3 = Encrypted and Signed\n");
    printf("\t--signed-recpt\n");
    printf("\t\tRequests a signed receipt when sending.\n");
    printf("\t--ssl\n");
    printf("\t\tEnabled SSL when the server is started. Clients must connect over SSL.\n");
    printf("\n\n");
    printf("Example (plaintext): oftpserver.exe\n");
    printf("Example (ssl): oftpserver.exe --download-dir C:\\oftp\\incoming --cert C:\\somewhere\\myfile.pfx --certpass mypass --ssl\n");
    printf("Example (ssl + signed + encrypted): oftpserver.exe --cert C:\\somewhere\\myfile.pfx --certpass mypass --ssl --recipient-cert C:\\somewhere\\partner.cer --file-security 3 --signed-recpt\n");
    printf("Note: The password for included oftpserver.pfx files is \"test\"\n");
    printf("************************************************************\n");
    printf("\n\n");
}

int main(int argc, char* argv[]) {
  int ret_code = 0;
  MyOFTPServer server;
  
  
  PrintUsage();

  server.SetServerSSIDCode(SERVER_ID);
  server.SetServerSFIDCode(SERVER_ID);
  server.SetCertStoreType(CST_AUTO);
  server.SetServerPassword(SERVER_PASS);

  //Parse arguments
  char* cert = GetArg("--cert", argc, argv);
  recipientCert = GetArg("--recipient-cert", argc, argv); //global variable used inside events
  if (ArgExists("--file-security", argc, argv)) fileSecurityLevel = atoi(GetArg("--file-security", argc, argv));
  if (ArgExists("--signed-recpt", argc, argv)) signedReceipt = true;
  if (!ArgExists("--download-dir", argc, argv)) {
      printf("--download-dir MUST be specified.\n");
      goto error;
  }
  DownloadDir = GetArg("--download-dir", argc, argv);
  OutgoingDir = GetArg("--outgoing-dir", argc, argv);

  if (strlen(cert)) {
      //The certificate with private key is used to decrypt incoming messages and sign outgoing messages.
      //In this demo the same certificate will also be used to set SSLCert to ssl connections
      server.SetCertStoreType(CST_AUTO);
      server.SetCertStore(cert, strlen(cert));
      server.SetCertStorePassword(GetArg("--certpass", argc, argv));
      ret_code = server.SetCertSubject("*");
      if (ret_code) goto error;

      //Use the same certificate for SSL connections (controlled by the UseSSL property)
      server.SetSSLCertStoreType(CST_AUTO);
      server.SetSSLCertStore(cert, strlen(cert));
      server.SetSSLCertStorePassword(GetArg("--certpass", argc, argv));
      ret_code = server.SetSSLCertSubject("*");
      if (ret_code) goto error;

      char logmsg[MAX_PATH];
      sprintf(logmsg, "Certificate with subject \"%s\" set.", server.GetCertSubject());
      Log(logmsg);
      
  }
  if (ArgExists("--ssl", argc, argv)) {
      //If SSL is set, a valid certificate with private key must be supplied as well.
      if (strlen(cert)) {
          Log("SSL Enabled.");
          server.SetUseSSL(true);
      }
      else {
          printf("When SSL is enabled the --cert parameter must be specified.\n");
          return 0;
      }
  }

  

  Log("Starting server ...");

  ret_code = server.SetListening(true);
  if (ret_code) goto error;

  char logmsg[MAX_PATH];
  sprintf(logmsg, "Server listening on %s:%d", server.GetLocalHost(), server.GetLocalPort());
  Log(logmsg);

  while (true && !readyToSend)
      server.DoEvents();

  //When readyToSend becomes true we will send files to the client and close the connection
  ret_code = SendFilesToClient(server);
  if (ret_code) goto error;

error:
  if (server.GetLastErrorCode()) {
      printf("Error [%d] : %s\n", server.GetLastErrorCode(), server.GetLastError());
  }

  
  
  printf("\n(press any key to exit)");
  getchar();
  return 0;
}



