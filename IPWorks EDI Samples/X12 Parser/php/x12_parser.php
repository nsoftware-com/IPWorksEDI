<?php $sendBuffer = TRUE; ob_start(); ?>
<html>
<head>
<title>IPWorks EDI 2022 Demos - X12 Parser</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="IPWorks EDI 2022 Demos - X12 Parser">
</head>

<body>

<div id="content">
<h1>IPWorks EDI - Demo Pages</h1>
<h2>X12 Parser</h2>
<p>Shows how to parse EDI X12 document via the ParseFile or Input methods.</p>
<a href="default.php">[Other Demos]</a>
<hr/>

<?php
require_once('../include/ipworksedi_x12reader.php');
require_once('../include/ipworksedi_x12writer.php');
require_once('../include/ipworksedi_const.php');

?>

<html>
<head>
<title>X12 Integrator 2022 Demos - EDI Parser</title>
<link rel="stylesheet" type="text/css" href="stylesheet.css">
<meta name="description" content="X12 Integrator 2022 Demos - EDI Parser"></head>

<body>

<div id="content">
<h1>X12 Integrator - Demo Pages</h1>
<h2>EDI Parser</h2>
<p>Shows how to parse EDI X12 document via the ParseFile or Input methods.</p>
<a href="seecode.php?ediparser.php">[See The Code]</a>
<a href="default.php">[Other Demos]</a>
<a href="inx12.chm">[Help]</a>
<hr/>

<?php
require_once('../include/ipworksedi_x12reader.php');
require_once('../include/ipworksedi_x12writer.php');
?>



<?php
	$output='';
	class MyEdiReader extends IPWorksEDI_X12Reader {
		
    	function fireEndFunctionalGroup($param){			
			global $output;
			$output.="EndFunctionalGroup: " . $param['tag'] . "\r\n";             				
    	}

		function fireEndInterchange($param){			
			global $output;
			$output.="EndInterchange: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireEndLoop($param){			
			global $output;
			$output.="EndLoop \r\n";             				
    	}
		
		function fireEndTransaction($param){			
			global $output;
			$output.="EndTransaction: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireError($param){			
			global $output;
			$output.="ERROR " . $param['errorcode'] . ":" . $param['description'] . "\r\n";             				
    	}
		
		function fireResolveSchema($param){			
			global $output;
			$output.="ResolveSchema: " . $param['transactioncode'] . "\r\n";             				
    	}
		
		function fireSegment($param){			
			global $output;
			$output.="Segment: " . $param['name'] . "\r\n";             				
    	}
		
		function fireStartFunctionalGroup($param){			
			global $output;
			$output.="StartFunctionalGroup: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireStartInterchange($param){			
			global $output;
			$output.="StartInterchange: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireStartLoop($param){			
			global $output;
			$output.="StartLoop: " . $param['name'] . "\r\n";             				
    	}
		function fireStartTransaction($param){			
			global $output;
			$output.="StartTransaction: " . $param['tag'] . "\r\n";             				
    	}
		
		function fireWarning($param){			
			global $output;
			$output.="WARNING" . $param['warncode'] . ":" + $param['message'] . "\r\n";             				
    	}
  };
  
  $edireader1 = new MyEdiReader();

?>

<?php
if ($_SERVER['REQUEST_METHOD'] == "POST") {

   try{
		$edireader1->doConfig("ResolveXPathOnSet=true");
        $edireader1->doConfig("Encoding=iso-8859-1");
        $edireader1->doLoadSchema(getcwd() . "/RSSBus_00401_810.json");
		$edireader1->setInputData($_POST["input"]);
        $edireader1->doParse();
	    
	}catch (Exception $ex){
		echo 'Error message: ', $edireader1->lastErrorCode() , ' :', $edireader1->lastError();
	}

}
?>

<form method=POST>
<center>
<table width="90%">
<tr><td><b>EDI Data:</b></td><td><b>Parsed Result:</b></td>
<tr><td><textarea name=input cols=110 rows=22>
ISA*00*          *00*          *ZZ*ACME           *ZZ*WAYNE_TECH     *160707*1544*U*00401*000000006*0*T*>~
GS*IN*ACME*WAYNE_TECH*20160707*1544*6*T*004010~
ST*810*0001~
BIG*20150708*3003014445**0476553272***DR~
CUR*SE*USD~
REF*8M*0056~
N1*BY*Company*92*544380~
N3*Address~
N4*City*CA*Postal Code~
N1*ST*Name*92*0607047800010~
N3*Address~
N4*City**200131*US~
N1*RE*Name*92*5095956~
N3*Address~
N4*City*IL*Postal Code~
IT1*20*2500*EA*36.96**BP*335S0594~
REF*KK*0099778154~
REF*PO*0476553272*20~
TDS*9240000~
CTT*1~
SE*19*0001~
GE*1*6~
IEA*1*000000006~

            
</textarea>
<td><textarea name=output cols=110 rows=22>
<?php echo $output ?>
</textarea>

<tr><td><input type=submit value="Parse"><td>

</table>
</center>


</form>

<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the X12 Integrator objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-BMPBA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2022 /n software inc. - All rights reserved.
<br/>
<br/></div>

<div id="footer">
<center>
X12 Integrator 2022 - Copyright (c) 2022 /n software inc. - All rights reserved. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-BMPBA" target="_blank">www.nsoftware.com</a>.</center></div>
</body></html>


<br/>
<br/>
<br/>
<hr/>
NOTE: These pages are simple demos, and by no means complete applications.  They
are intended to illustrate the usage of the IPWorks EDI objects in a simple,
straightforward way.  What we are hoping to demonstrate is how simple it is to
program with our components.  If you want to know more about them, or if you have
questions, please visit <a href="http://www.nsoftware.com/?demopg-BEPHA" target="_blank">www.nsoftware.com</a> or
contact our technical <a href="http://www.nsoftware.com/support/">support</a>.
<br/>
<br/>
Copyright (c) 2023 /n software inc.
<br/>
<br/>
</div>

<div id="footer">
<center>
IPWorks EDI 2022 - Copyright (c) 2023 /n software inc. - For more information, please visit our website at <a href="http://www.nsoftware.com/?demopg-BEPHA" target="_blank">www.nsoftware.com</a>.
</center>
</div>

</body>
</html>

<?php if ($sendBuffer) ob_end_flush(); else ob_end_clean(); ?>
