$(document).on('shiny:inputchanged', function(event) {
  if (event.name === 'profile') {
	  //Dropdown boxes
	 levelValue = document.getElementById("level").value 
	 locValue = document.getElementById("unit").value

     //Checkbox series
	  
      var inputElements = document.getElementsByClassName('outChk');
      if(inputElements[0].checked) {
		 var basic = 'yes'
	  } else {
		 var basic = 'no'
	  }		 
	  if(inputElements[1].checked) {
		 var popf = 'yes'
	  } else {
		 var popf = 'no'
	  }		 
     if(inputElements[2].checked) {
		 var popa = 'yes'
	  } else {
		 var popa = 'no'
	  }		 
     if(inputElements[3].checked) {
		 var popc = 'yes'
	  } else {
		 var popc =  'no'
	  }		 
     if(inputElements[4].checked) {
		 var house = 'yes'
	  } else {
		 var house = 'no'
	  }		 
     if(inputElements[5].checked) {
		 var comm =  'yes'
	  } else {
		 var comm = 'no'
	  }		 
     if(inputElements[6].checked) {
		 var empli = 'yes'
	  } else {
		 var empli = 'no'
	  }		 
     if(inputElements[7].checked) {
		 var emplc =  'yes'
	  } else {
		 var emplc = 'no'
	  }		 

    //Writing dataLayer.push
	window.dataLayer = window.dataLayer || [];
    window.dataLayer.push({ event: 'ProfileSubmit',
	      'DataLevel' : levelValue,
		  'Location' : locValue,
	      'BasicStatistics' : basic,
		  'PopulationForecast' : popf,
          'AgeCharacteristics' : popa,
          'PopulationCharacteristics' : popc,
		  'Housing' : house,
          'Commuting' : comm,
          'EmploymentIndustry' : empli,
	      'EmploymentCharacteristics' : emplc });
		  
  }