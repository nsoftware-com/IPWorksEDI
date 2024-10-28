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

program as2server;

uses
  Forms,
  certmgrf in 'certmgrf.pas'   {FormCertmgrf},
  as2serverf in 'as2serverf.pas' {FormAs2server};

begin
  Application.Initialize;

  Application.CreateForm(TFormAs2server, FormAs2server);
  Application.CreateForm(TFormCertmgr, FormCertmgr);

  Application.Run;
end.


         
