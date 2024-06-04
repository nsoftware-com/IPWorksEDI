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
#define CLIENT_ID "CLIENTSSID"
#define CLIENT_PASS "PASSWORD"


char input[MAX_PATH + 1];
char* recipientCert;
bool readyToSend = false;
int clientConnectionId = -1;
bool signedReceipt = false;
int fileSecurityLevel = -1;

char* GetArg(const char* option, int argc, char** argv) {
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

    printf("[%s] %s\n", buffer, message);
}

const char* prompt(const char* prompt, const char* defaultVal) {
    printf("%s [%s]: ", prompt, defaultVal);
    fgets(input, MAX_PATH, stdin);
    input[strlen(input) - 1] = '\0';
    if (strlen(input) == 0) {
        strncpy_s(input, defaultVal, MAX_PATH);
        input[MAX_PATH] = '\0';
    }
    return input;
}

class MyOFTPClient : public OFTPClient {
public:
    virtual int FireSSLServerAuthentication(OFTPClientSSLServerAuthenticationEventParams* e) {
        e->Accept = true;
        return 0;
    }
    virtual int FireAcceptFile(OFTPClientAcceptFileEventParams* e) {
        e->Overwrite = true;
        return 0;
    }
    virtual int FireStartTransfer(OFTPClientStartTransferEventParams* e) {

        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "Started sending file \"%s\" to \"%s\" ...", e->VirtualFileName, e->Destination);

        }
        else {
            sprintf(logmsg, "Recieving file \"%s\" ...", e->VirtualFileName);
        }

        Log(logmsg);
        return 0;
    }
    virtual int FireEndTransfer(OFTPClientEndTransferEventParams* e) {
        char logmsg[MAX_PATH];

        if (e->Direction == 0) {
            sprintf(logmsg, "Sent file \"%s\" to \"%s\".", e->VirtualFileName, e->Destination);
        }
        else {
            sprintf(logmsg, "Received file \"%s\".", e->VirtualFileName);
        }

        Log(logmsg);
        return 0;
    }
    virtual int FireEndResponse(OFTPClientEndResponseEventParams* e) {
        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "Sent End Response for %s.", e->VirtualFileName);
        }
        else {
            sprintf(logmsg, "Recieved End Response for %s.", e->VirtualFileName);
        }
        Log(logmsg);
        return 0;
    }

    virtual int FirePITrail(OFTPClientPITrailEventParams* e) {
        char logmsg[MAX_PATH];
        if (e->Direction == 0) {
            sprintf(logmsg, "SERVER: %s", e->CommandDescription);
        }
        else {
            sprintf(logmsg, "CLIENT: %s", e->CommandDescription);
        }

        // Uncomment to enable protocol level logging
        //Log(logmsg);
        return 0;
    }
};

void PrintUsage() {
    printf("************************************************************\n");
    printf("The command line arguments below control the operation of the client. The client can send and receive files with optional signing and encrypting.\n");
    printf("\n");
    printf("usage: oftpclient.exe server port [--file] [--cert] [--certpass] [--recipient-cert] [--file-security] [--signed-recipt] [--ssl] [--download-dir]\n");
    printf("\n");
    printf("Parameter details:\n");
    printf("\tserver\n");
    printf("\t\tThe address of the remote host.\n");
    printf("\tport\n");
    printf("\t\tThe port on the remote host.\n");
    printf("\t--file C:\\oftp\\myfile.txt\n");
    printf("\t\tSpecifies the path to the file to send.\n");
    printf("\t--cert C:\\somewhere\\myfile.pfx\n");
    printf("\t\tSpecifies the certificate with private key for signing, decryption, and ssl.\n");
    printf("\t--certpass mypassword\n");
    printf("\t\tSpecifies the password for the certificate specified in --cert\n");
    printf("\t--recipient-cert C:\\somewhere\\recipient.cer\n");
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
    printf("\t\tEnables SSL when the client is started.\n");
    printf("\t--download-dir\n");
    printf("\t\tThe directory to which downloaded files are saved.\n");

    printf("\n\n");
    printf("Example (plaintext send): oftpclient.exe 192.168.0.1 3305 test.txt\n");
    printf("Example (ssl): oftpclient.exe 192.168.0.1 3305 test.txt --cert C:\\somewhere\\myfile.pfx --certpass mypass --ssl\n");
    printf("Example (ssl + signed + encrypted): oftpclient.exe 92.168.0.1 3305 test.txt --cert C:\\somewhere\\myfile.pfx --certpass mypass --ssl --recipient-cert C:\\somewhere\\partner.cer --file-security 3 --signed-recpt\n");
    printf("Example (plaintext download): oftpclient.exe 192.168.0.1 3305 --download --download-dir C:\\oftp\\incoming\n");
    printf("Note: The password for included oftpserver.pfx files is \"test\"\n");
    printf("************************************************************\n");
    printf("\n\n");
}

int main(int argc, char* argv[])
{
    int ret_code = 0;
    MyOFTPClient client;
    PrintUsage();
    if(argc >= 4) {
        if (client.SetRemoteHost(argv[1])) goto error;


        client.SetServerSSIDCode(SERVER_ID);
        client.SetServerSFIDCode(SERVER_ID);
        client.SetServerPassword(SERVER_PASS);

        client.SetClientSSIDCode(CLIENT_ID);
        client.SetClientSFIDCode(CLIENT_ID);
        client.SetClientPassword(CLIENT_PASS);

        //Parse Arguments
        char* cert = GetArg("--cert", argc, argv);
        if (strlen(cert)) {
            //The certificate with private key is used to decrypt incoming messages and sign outgoing messages.
            //In this demo the same certificate will also be used to set SSLCert to ssl connections
            client.SetCertStoreType(CST_AUTO);
            client.SetCertStore(cert, strlen(cert));
            client.SetCertStorePassword(GetArg("--certpass", argc, argv));
            ret_code = client.SetCertSubject("*");
            if (ret_code) goto error;

            //Use the same certificate for SSL connections (controlled by the UseSSL property)
            client.SetSSLCertStoreType(CST_AUTO);
            client.SetSSLCertStore(cert, strlen(cert));
            client.SetSSLCertStorePassword(GetArg("--certpass", argc, argv));
            ret_code = client.SetSSLCertSubject("*");
            if (ret_code) goto error;

            char logmsg[MAX_PATH];
            sprintf(logmsg, "Certificate with subject \"%s\" set.", client.GetCertSubject());
            Log(logmsg);

        }
        char* recipient_cert = GetArg("--recipient-cert", argc, argv);
        //In this demo the recipient certificate password is the same as the other certificate passwords.
        if (strlen(recipient_cert)) {
            client.SetRecipientCertStoreType(CST_AUTO);
            client.SetRecipientCertStore(cert, strlen(cert));
            client.SetRecipientCertStorePassword(GetArg("--certpass", argc, argv));
            ret_code = client.SetRecipientCertSubject("*");
            if (ret_code) goto error;
        }
        if (ArgExists("--ssl", argc, argv)) {
            //If SSL is set, a valid certificate with private key must be supplied as well.
            if (strlen(cert)) {
                Log("SSL Enabled.");
                client.SetUseSSL(true);
            }
            else {
                printf("When SSL is enabled the --cert parameter must be specified.\n");
                return 0;
            }
        }
        if (ArgExists("--file-security", argc, argv)) 
            client.SetVirtualFileSecurityLevel(atoi(GetArg("--file-security", argc, argv)));
        if (ArgExists("--signed-recpt", argc, argv)) client.SetSignedReceipt(true);

        if (client.SetRemotePort(atoi(argv[2]))) goto error;

        

        Log("Starting Client ...");

        //Send file if specified
        if (ArgExists("--file", argc, argv)) {
            if (client.SendFile(GetArg("--file",argc,argv), "")) goto error;
        }

        //Download files if specified
        if (ArgExists("--download-dir", argc, argv)) {
            client.SetDownloadDirectory(GetArg("--download-dir", argc, argv));
            if (client.ReceiveFiles()) goto error;
        }

        
        Log("Ending session.");

error:
        if (client.GetLastErrorCode()) {
            printf("Error [%d] : %s\n", client.GetLastErrorCode(), client.GetLastError());
        }
        printf("\nPress enter to exit.");
        getchar();
    }
    return 0;
}

