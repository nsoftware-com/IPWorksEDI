
import java.util.*;
import java.io.*;

public class gnupg_provider {

	private Hashtable param = new Hashtable(); 
	private String errorString = "";
	private String outputString = "";
	private Runtime rt;
	private Process gpgProcess;
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
	}

	public void setParam(java.lang.String pname, java.lang.String pvalue){
		if( pname != null && pname != "" && pvalue != null ) {
			if ( pname == "homedir" ) {
				if ( !pvalue.startsWith("\"") ) pvalue = "\"" + pvalue;
				if ( !pvalue.endsWith("\"") ) pvalue = pvalue + "\"";
			}
			param.put(pname.toLowerCase(),pvalue);
		}
	}
	
	public byte[] encrypt(byte[] data) throws Exception{
		String options = "--encrypt -a ";
		String homedir = (String) param.get("homedir");
		String recipient = (String) param.get("recipient-userid");

		if( homedir != null && homedir != "" ) 
			options += "--homedir "+homedir+" ";

		if( recipient != null || recipient != "" )
			options += "--recipient "+recipient+" ";
		else
			throw new Exception("Recepient must be specified for encryption.");

		
		String inputData = new String(data);
		String outputText = executeCommand(options, inputData);
		return outputText.getBytes();
		
	}
	public byte[] decrypt(byte[] data) throws Exception{
		String options = "";
		String homedir = (String)param.get("homedir");
		String outputText = "";

		if( homedir != null && homedir != "" ) 
			options += "--homedir "+homedir+" ";
		
		options += " --decrypt ";

		options += "--passphrase-fd 0";
			
		String inputData = new String(data);
		try{
		outputText = executeCommand(options, inputData);
		}
		catch(Exception ex){
			throw new Exception(ex.getMessage());
		}

		return outputText.getBytes();
		
	}
	public byte[] sign(byte[] data) throws Exception{
		String options = "";
		String userid = (String)param.get("userid");
		String homedir = (String)param.get("homedir");
		String outputText = "";

		if( homedir != null && homedir != "" ) 
			options += "--homedir "+homedir+" ";
			
		if( userid != null && userid != "" ) 
			options += "--local-user "+userid+" ";
				
		options += "--detach-sign -a "; 
		options += " --passphrase-fd 0";

		String inputData = new String(data);
		try{
		outputText = executeCommand(options, inputData);
		}
		catch(Exception exc)
		{
			throw new Exception(exc.getMessage());
		}
		return outputText.getBytes();
		
	}
	
	public boolean verifySignature(byte[] signature,byte[] signedData) {
		String options = "";
		String passphrase = (String)param.get("passphrase");
		
		String homedir = (String)param.get("homedir");
		if( homedir != null && homedir != "" ) 
			options += "--homedir "+homedir+" ";

		try{
		String tempFileSig = "tempfilesig.txt";
		File outputFile = new File(tempFileSig);
		FileWriter out = new FileWriter(outputFile.getAbsoluteFile());
		out.write(new String(signature));
		out.close();
		
		String tempFileData = "tempfiledata.txt";
		File outputFileData = new File(tempFileData);
		out = new FileWriter(outputFileData.getAbsoluteFile());
		out.write(new String(signedData));
		
		
		options += " --verify ";
		options += "\"" + tempFileSig + "\" \"" + tempFileData + "\"";
		String inputData = "";
		String outputText = "";
		
		// We dont want to write the passphrase when we verify signatures
		if( passphrase != null ) param.remove("passphrase");
		outputText = executeCommand(options, inputData);
		if( passphrase != null ) param.put("passphrase", passphrase);
		
		outputFile.delete();
		outputFileData.delete();
		}
		catch (Exception ex) { }
		return true; // if the signature is bad then executeCommand will throw an exception.
	}
	public String executeCommand(String gpgOptions, String inputText) throws Exception{
		String gpgPath = (String)param.get("gpg-path");
		String passphrase = (String)param.get("passphrase");
		String line = "";

		if( gpgPath == null || gpgPath == "" )
			throw new Exception("gpg-path must be specifed to use the GnuPG provider.");

		rt = Runtime.getRuntime();
		gpgProcess = rt.exec(gpgPath + " " + gpgOptions);
		
		OutputStream os= gpgProcess.getOutputStream();
		OutputStream buffos= new BufferedOutputStream(os);
		OutputStreamWriter out = new OutputStreamWriter(buffos); 
		
		// Send pass phrase, if signing or decrypting
		if (passphrase != null && passphrase != "" && 
				(gpgOptions.indexOf("sign") != -1 || gpgOptions.indexOf("decrypt") != -1) ) {
			out.write(passphrase + "\r\n");
			out.flush();
		}

		// Send input text
		out.write(inputText);
		out.flush();
		out.close();

		outputString = "";
		errorString = "";

		InputStream buffis = new BufferedInputStream(gpgProcess.getInputStream());
		InputStreamReader instrm = new InputStreamReader(buffis);
		BufferedReader in = new BufferedReader(instrm);
		
		do{
			try{
				line = (String) in.readLine();
			
			}
			catch(IOException ex)
			{
			}
		if(line != null)
			outputString += line + "\r\n";
		}while (line != null);
		outputString = outputString.substring(0,outputString.length()-2);
	    	// Check results and prepare output
		int exitcode = gpgProcess.exitValue();
		if( exitcode != 0 ) {
			if (errorString == "") errorString = "GnuPG : [" + String.valueOf(exitcode) + "]: Unknown error";
			throw new Exception(errorString);
		}

		return outputString;
	}
}
