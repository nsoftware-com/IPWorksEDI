/*
 * IPWorks EDI 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksedi = require("@nsoftware/ipworksedi");

if(!ipworksedi) {
  console.error("Cannot find ipworksedi.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

main();

async function main() {
  console.log("**********************************************************");
  console.log("* This demo shows how to use the SFTPServer component to *");
  console.log("* create a simple SFTP Server.                           *");
  console.log("* Use the following credentials to connect.              *");
  console.log("* User: test                                             *");
  console.log("* Password: test                                         *");
  console.log("**********************************************************");

  const sftpserver = new ipworksedi.sftpserver();

  // For the purposes of this demo we are using the included certificate.
  // You may change these options to specify your own certificate.
  // See the documentation for additional information.
  const cert = new ipworksedi.Certificate(ipworksedi.CertStoreTypes.cstPFXFile, "sftpserver.pfx", "demo", "*")
  sftpserver.setSSHCert(cert);

  // Set up event handling. There are additional events not implemented here; see the
  // documentation for details.
  sftpserver.on("Connected", (e) => (console.log(`[${e.connectionId}] Now Connected`)))
  .on("ConnectionRequest", (e) => (console.log(`${e.address}:${e.port} is attempting to connect.`)))
  .on("DirCreate", (e) => (console.log(`${e.user} created the directory ${e.path}`)))
  .on("DirRemove", (e) => (console.log(`${e.user} deleted the directory ${e.path}`)))
  .on("Disconnected", (e) => (console.log(`[${e.connectionId}] Now Disconnected`)))
  .on("Error", (e) => (console.log(`[${e.connectionId}] Error:${e.description}`)))
  .on("FileClose", (e) => (console.log(`${e.user} transferred ${e.path}`)))
  .on("FileOpen", (e) => {
    let operation = "";
    if ((e.flags & 1) !== 0) {
      // Read
      operation = "downloading";
    }
    if (e.flags % 2 !== 0) {
      operation = "uploading";
    }
    console.log( `${e.user} started ${operation} ${e.path}` );
  })
  .on("FileRemove", (e) => (console.log(`${e.user} deleted the file ${e.path}`)))
  .on("FileRename", (e) => (console.log(`${e.user} renamed the file ${e.path}`)))
  .on("SSHStatus", (e) => (console.log(`[${e.connectionId}] ${e.message}`)))
  .on("SSHUserAuthRequest", (e) => {
    if (e.user === "test" && e.authMethod !== "none" && e.authParam === "test") {
      e.accept = true;
      console.log(`${e.user} has successfully authenticated.`);
    }
  });

  // Prompt user.
  prompt("rootdirectory", "Root Directory", ":", "./");

  rl.on("line", async function (line) {
    switch (lastPrompt) {
      case "rootdirectory": {
        sftpserver.setRootDirectory(line === "" ? lastDefault : line);
        prompt("localport", "Local Port", ":", "22");
        break;
      }
      case "localport": {
        sftpserver.setLocalPort(Number(line === "" ? lastDefault : line));
        try {
          await sftpserver.startListening();
          console.log(`Server listening on port ${sftpserver.getLocalPort()}.`);
          prompt("exit", "Q to exit", ":", "");
        } catch (ex) {
          console.log(ex);
          process.exit();
        }

        break;
      }
      case "exit": {
        if (line === "q" || line === "Q") {
          console.log("Server shutting down. Goodbye!");
          await sftpserver.shutdown();
          process.exit();
        }
      }
    }
  });
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}] ${punctuation} `);
}
