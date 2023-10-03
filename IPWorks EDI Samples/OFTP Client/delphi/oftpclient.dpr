(*
 * IPWorks EDI 2022 Delphi Edition - Sample Project
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

program oftpclient;

uses
  Forms,
  oftpclientf in 'oftpclientf.pas' {FormOftpclient};

begin
  Application.Initialize;

  Application.CreateForm(TFormOftpclient, FormOftpclient);
  Application.Run;
end.


         
