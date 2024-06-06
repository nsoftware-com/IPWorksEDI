import SwiftUI
import IPWorksEDI

struct ContentView: View, SFTPServerDelegate {
  @State private var outputRes: String = ""
  
  func onConnected(connectionId: Int32, statusCode: Int32, description: String, certStoreType: inout Int32, certStore: inout String, certPassword: inout String, certSubject: inout String) {}    
  func onConnectionRequest(address: String, port: Int32, accept: inout Bool) {}    
  func onDirCreate(connectionId: Int32, user: String, path: String, fileType: Int32, fileSize: Int64, fileOwner: String, fileGroup: String, filePermissions: Int32, fileATime: Int64, fileCreateTime: Int64, fileMTime: Int64, fileAttribBits: Int32, fileAttribBitsValid: Int32, otherAttributes: String, beforeExec: Bool, statusCode: inout Int32) {}    
  func onDirList(connectionId: Int32, user: String, path: String, beforeExec: Bool, statusCode: inout Int32) {}    
  func onDirRemove(connectionId: Int32, user: String, path: String, beforeExec: Bool, statusCode: inout Int32) {}    
  func onDisconnected(connectionId: Int32, statusCode: Int32, description: String) {}    
  func onError(connectionId: Int32, errorCode: Int32, description: String) {
    outputRes += description
  }    
  func onFileClose(connectionId: Int32, user: String, path: String, handle: String, statusCode: inout Int32) {}    
  func onFileOpen(connectionId: Int32, user: String, path: String, desiredAccess: Int32, flags: Int32, fileType: Int32, fileSize: Int64, fileOwner: String, fileGroup: String, filePermissions: Int32, fileATime: Int64, fileCreateTime: Int64, fileMTime: Int64, fileAttribBits: Int32, fileAttribBitsValid: Int32, otherAttributes: String, handle: inout String, beforeExec: Bool, statusCode: inout Int32) {}    
  func onFileRead(connectionId: Int32, user: String, handle: String, fileOffset: Int64, length: Int32, statusCode: inout Int32) {}    
  func onFileRemove(connectionId: Int32, user: String, path: String, beforeExec: Bool, statusCode: inout Int32) {}
  func onFileRename(connectionId: Int32, user: String, path: String, newPath: String, flags: Int32, beforeExec: Bool, statusCode: inout Int32) {}    
  func onFileWrite(connectionId: Int32, user: String, handle: String, fileOffset: Int64, beforeExec: Bool, statusCode: inout Int32) {}    
  func onGetAttributes(connectionId: Int32, user: String, path: String, flags: Int32, fileType: inout Int32, fileSize: inout Int64, fileOwner: inout String, fileGroup: inout String, filePermissions: inout Int32, fileATime: inout Int64, fileCreateTime: inout Int64, fileMTime: inout Int64, fileAttribBits: inout Int32, fileAttribBitsValid: inout Int32, otherAttributes: inout String, statusCode: inout Int32) {}    
  func onLog(connectionId: Int32, logLevel: Int32, message: String, logType: String) {
    outputRes += message + "\n"
  }    
  func onResolvePath(connectionId: Int32, user: String, originalPath: String, controlFlags: Int32, realPath: inout String, statusCode: inout Int32) {}    
  func onSetAttributes(connectionId: Int32, user: String, path: String, fileType: Int32, fileSize: Int64, fileOwner: String, fileGroup: String, filePermissions: Int32, fileATime: Int64, fileCreateTime: Int64, fileMTime: Int64, fileAttribBits: Int32, fileAttribBitsValid: Int32, otherAttributes: String, beforeExec: Bool, statusCode: inout Int32) {}    
  func onSSHStatus(connectionId: Int32, message: String) {}    
  func onSSHUserAuthRequest(connectionId: Int32, user: String, service: String, authMethod: String, authParam: String, accept: inout Bool, partialSuccess: inout Bool, availableMethods: inout String, homeDir: inout String, keyAlgorithm: String) {
    if authMethod == "none" {return}
    
    if user == "test" && authParam == "test" {
      accept = true
      outputRes+=user + " has successfully authenticated."
    }
  }    
  
  var server = SFTPServer()    
  var documentsPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] + "/"
  @State private var connected = false
  
  func connectedChange() -> String
  {
    if (connected)
    {
      return "Stop Server"
    }
    else
    {
      return "Start Server"
    }
  }
  
  var body: some View {
    VStack(alignment: .center)
    {
      Text("Simple SFTP Server demo. Click Start Server to listen on port 22.")
        .foregroundColor(Color.blue)
      startButton()          
      Text("Log:")
      TextEditor(text: $outputRes)
        .border(Color.black, width: 1)
    }
    .padding(/*@START_MENU_TOKEN@*/.all, 5.0/*@END_MENU_TOKEN@*/)
  }
  
  @ViewBuilder
  private func startButton() -> some View {
    Button(action:
            {
      //client.runtimeLicense = ""
      server.delegate = self
      outputRes = ""      
      do
      {
        if (server.listening)
        {
          try server.stopListening()
        }
        else
        {
          server.sshCert = Certificate(storeType: CertStoreTypes.cstPFXFile, store: "/Applications/IPWorks EDI 2024 macOS Edition/demos/SFTP Server/sftpserver.pfx", storePassword: "demo", subject: "*")
          try server.startListening()
          outputRes+="Server listening at: "+server.localHost+" port: "+String(server.localPort)+"\nLog in with user: test password: test\n"
        }
      }
      catch
      {
        do
        {
          try server.stopListening()
        }
        catch {}
        outputRes += "Error: \(error)"
        return
      }
    })
    {
      Text("\(connectedChange())")
        .font(.system(size: 20))
        .frame(minWidth: 150, minHeight: 40)
        .background(RoundedRectangle(cornerRadius: 8)
          .fill(Color.gray))
    }
    .buttonStyle(PlainButtonStyle())
    
  }
  
}

struct ContentView_Previews: PreviewProvider {
  static var previews: some View {
    ContentView()
  }
}
