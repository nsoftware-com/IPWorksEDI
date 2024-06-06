(*
 * IPWorks EDI 2024 Delphi Edition - Sample Project
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
 *)

program oftpserver;

uses
  Forms,
  certmgrf in 'certmgrf.pas'   {FormCertmgrf},
  oftpserverf in 'oftpserverf.pas' {FormOftpserver};

begin
  Application.Initialize;

  Application.CreateForm(TFormOftpserver, FormOftpserver);
  Application.CreateForm(TFormCertmgr, FormCertmgr);

  Application.Run;
end.


         
