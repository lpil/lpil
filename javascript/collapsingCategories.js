//collapsing categories version 2.0.0.0
 $(document).ready(function() 
    {

jQuery.expr[':'].regex = function(elem, index, match) {
    var matchParams = match[3].split(','),
        validLabels = /^(data|css):/,
        attr = {
            method: matchParams[0].match(validLabels) ? 
                        matchParams[0].split(':')[0] : 'attr',
            property: matchParams.shift().replace(validLabels,'')
        },
        regexFlags = 'ig',
        regex = new RegExp(matchParams.join('').replace(/^\s+|\s+$/g,''), regexFlags);
    return regex.test(jQuery(elem)[attr.method](attr.property));
}

	//collapse all categories, except the one that is selected
	var L1=0;
	var L2=0;
	var L3=0;
	var L4=0;
	var L5=0;
	var selected=0;
	var clicked=0;
	//run through all the cells looking for the right classes
	$("td[class^=categorySidebarLabelLevel]").each(function(index) 
	  {
		if(this.className=='categorySidebarLabelLevel1 categorySidebarLabel' || this.className=='categorySidebarLabelLevel1 categorySidebarLabelSelected')
		{
			//found level 1 cell, save and increment the count
			L1=L1*1+1;
			//reset lower levels
			L2=0;
			L3=0;
			L4=0;
			L5=0;
			//add an ID to the cell
			$(this).attr("id",L1); 

			//add an image before the hyperlink
			$(this).children("a").before("<img src='Custom/Themes/sjp_2014_06/javascript/images/cc_expand.gif' border='0' id='exp_" + L1 + "'/> ");

			//add a collapse function to hide all lower levels
  			   $("#exp_" + L1).click(function() 
			   { 	
				myID=$(this).attr("id");
				myID=myID.replace("exp_","");
				clicked=myID;
				//toggle image
				if($(this).attr("src").indexOf("cc_expand.gif")>=1)
				{
					//image is the plus sign, change to collapse and show all level2
					$(this).attr("src","Custom/Themes/sjp_2014_06/javascript/images/cc_collapse.gif");
					$("[id^=" + myID + "_]:not( :regex(id,^[" + myID + "]+_+[0-9]+_) )").parent().show();
					$("[id^=" + myID + "_]:not( :regex(id,^[" + myID + "]+_+[0-9]+_) )").show();
					
				}
				else
				{
					//
					$(this).attr("src","Custom/Themes/sjp_2014_06/javascript/images/cc_expand.gif");
					$("[id^=" + myID + "_]").parent().hide();
					$("[id^=" + myID + "_]").hide();
					//also change all level2 images to plus sign ready for when they become visible again
					$("[id^=exp_" + myID + "_]").attr("src", "images/cc_expand.gif");
				}	
					
			    });
		}
		if(this.className=='categorySidebarLabelLevel2 categorySidebarLabel' || this.className=='categorySidebarLabelLevel2 categorySidebarLabelSelected')
		{
			//Save L2 count
			L2=L2*1+1;
			//reset lower levels
			L3=0;
			L4=0;
			//add an ID to the cell
			$(this).attr("id",L1 + "_" + L2); 
			$(this).hide();
			//the hyperlinks have no ID so give it one we can use to collapse

			$(this).children("a").attr("id","collapse_" + L1 + "_" + L2); 

			if(this.className=='categorySidebarLabelLevel2 categorySidebarLabelSelected')
  			  selected=L1;

			//add an image before the hyperlink
			$(this).children("a").before("<img src='images/cc_expand.gif' border='0' id='exp_" + L1 + "_" + L2 + "'/> ");

			//add a collapse function to hide all lower levels
  			   $("#exp_" + L1 + "_" + L2).click(function() 
			   { 
				myID=$(this).attr("id");
				myID=myID.replace("exp_","");
				clicked=myID;
				//toggle image
				if($(this).attr("src").indexOf("cc_expand.gif")>=1)
				{
					$(this).attr("src","images/cc_collapse.gif");
					$("[id^=" + myID + "_]:not( :regex(id,^[" + myID + "]+_+[0-9]+_+[0-9]+_) )").parent().show();
					$("[id^=" + myID + "_]:not( :regex(id,^[" + myID + "]+_+[0-9]+_+[0-9]+_) )").show();
				}
				else
				{
					$(this).attr("src","images/cc_expand.gif");
					$("[id^=" + myID + "_]").parent().hide();
					$("[id^=" + myID + "_]").hide();
				}				
					
			    });
		}

		if(this.className=='categorySidebarLabelLevel3 categorySidebarLabel' || this.className=='categorySidebarLabelLevel3 categorySidebarLabelSelected')
		{
			//Save L3 count
			L3=L3*1+1;
			//reset lower levels
			L4=0;
			L5=0;
			//add an ID to the cell
			$(this).attr("id",L1 + "_" + L2 + "_" + L3); 

			//the hyperlinks have no ID so give it one we can use to collapse
			$(this).children("a").attr("id","collapse_" + L1 + "_" + L2 + "_" + L3); 
    		        $(this).parent().hide();
    		        $(this).hide();
			if(this.className=='categorySidebarLabelLevel3 categorySidebarLabelSelected')
  			  selected=L1 + "_" + L2;
		}
		if(this.className=='categorySidebarLabelLevel4 categorySidebarLabel' || this.className=='categorySidebarLabelLevel4 categorySidebarLabelSelected')
		{
			//Save L4 count
			L4=L4*1+1;
			//reset lower levels
			L5=0;
			//add an ID to the cell
			$(this).attr("id",L1 + "_" + L2 + "_" + L3 + "_" + L4); 

			//the hyperlinks have no ID so give it one we can use to collapse
			$(this).children("a").attr("id","collapse_" + L1 + "_" + L2 + "_" + L3 + "_" + L4); 
 		        $(this).parent().hide();
 		        $(this).hide();
			if(this.className=='categorySidebarLabelLevel4 categorySidebarLabelSelected')
  			  selected=L1;
		}
		if(this.className=='categorySidebarLabelLevel5 categorySidebarLabel' || this.className=='categorySidebarLabelLevel5 categorySidebarLabelSelected')
		{
			//Save L5 count
			L5=L5*1+1;
			//reset lower levels
			//L6=0;
			//add an ID to the cell
			$(this).attr("id",L1 + "_" + L2 + "_" + L3 + "_" + L4 + "_" + L5); 

			//the hyperlinks have no ID so give it one we can use to collapse
			$(this).children("a").attr("id","collapse_" + L1 + "_" + L2 + "_" + L3 + "_" + L4 + "_" + L5); 
		        $(this).parent().hide();
		        $(this).hide();
			if(this.className=='categorySidebarLabelLevel5 categorySidebarLabelSelected')
  			  selected=L1;
		}		


	  }

        );
		//hide the plus image if there is no level2 hyperlinks
		L1=0;
		$("td[class^=categorySidebarLabelLevel1]").each(function(index) 
		  {
	  	    L1=L1*1+1;
	  	    if(!$("#collapse_" + L1 + "_1").length)
	   		{
				$("#exp_" + L1).hide();
				//look for L2 to hide...
			}
		for (i=0;i<=50;i++)
		{
	  	    if(!$("#collapse_" + L1 + "_" + i + "_1").length)
	   		{
				$("#exp_" + L1 + "_" + i).hide();
			}
			
		}



  		  });
		//expand selected tree
			if(selected!=0)
			{
			  //alert(selected);
			  document.getElementById("exp_" + selected).click();
			}
});