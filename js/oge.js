// @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL v3.0

document.body.style.background = "#E2F4FB"//"#E3EBF3";

// Filter only one value of the "select"-"group"
toggleFilterExclusive = function(value, obj){

}

toggleFilter = function(value, obj){
  var filterGroups = document.getElementsByClassName("main-filter-group");
  for (var k = 0; k < filterGroups.length; k++) {
    var filterGroup = filterGroups[k];
    var checkboxes = filterGroup.getElementsByClassName("crosstalk_checkbox");
  //  alert(checkboxes.length);
    for (var i = 0; i < checkboxes.length; i++) {
        var checkbox = checkboxes[i];
        if(checkbox.value == value && checkbox.checked != obj.checked) {
          checkbox.click();
        }
    }
  }
}


changeFun = function(value, obj){
 // alert('Changed value of '+value+" to "+obj.checked);
  
  var checkboxes = document.getElementsByClassName("crosstalk_checkbox");
  for (var i = 0; i < checkboxes.length; i++) {
      var checkbox = checkboxes[i];
      if(checkbox.value == value && checkbox.checked != obj.checked) {
        checkbox.checked = obj.checked;
      }
  }

/*var x = document.getElementsByClassName("crosstalk-input-checkboxgroup");
  for (var j = 0; j < x.length; j++) {
    var checkboxes = x[j].getElementsByTagName("input");
     var noneselected = true;
    for (var i = 0; i < checkboxes.length; i++) {
      var checkbox = checkboxes[i];
       //checkbox.parentNode.className = (checkbox.parentNode.className + " noneselected").trim();
      if(checkbox.value == value && checkbox.checked != obj.checked) {
        //checkbox.checked = obj.checked;
        checkbox.click();
        noneselected = noneselected && !(checkbox.checked);
      }

    }*/
    /*for (var i = 0; i < checkboxes.length; i++) {
      var checkbox = checkboxes[i];
      if(noneselected){
        //alert("noneselected!");
        checkbox.parentElement.className = (checkbox.parentElement.className + " noneselected").trim();
      } else {
        checkbox.parentElement.className = checkbox.parentElement.className.replace("noneselected","").trim();
      }
    }
  
  }*/
/*
var x = document.getElementsByClassName("crosstalk-input-checkboxgroup");
  for (var j = 0; j < x.length; j++) {
   // var noneselected = true;
    var checkboxes = x[j].getElementsByTagName("input");
    for (var i = 0; i < checkboxes.length; i++) {
      var checkbox = checkboxes[i];
      if(checkbox.value == value && checkbox.checked != obj.checked) {
        checkbox.checked = obj.checked;
      }
     // noneselected = noneselected && !(checkbox.checked);

    }

    /* for (var i = 0; i < checkboxes.length; i++) {
      if(noneselected){
        alert("noneselected!");
        checkbox[i].parentElement.className = (checkbox[i].parentElement.className + " noneselected").trim();
      } else {
        checkbox[i].parentElement.className = checkbox[i].parentElement.className.replace("noneselected","").trim();
      }
    }
  }*/

}


var filterGroups = document.getElementsByClassName("main-filter-group");

for (var k = 0; k < filterGroups.length; k++) {
  var filterGroup = filterGroups[k];
  var x = filterGroup.getElementsByClassName("crosstalk-input-checkboxgroup");
  var i;
  for (i = 0; i < x.length; i++) {
      var inputs = x[i].getElementsByTagName("input");
      for (var j = 0; j < inputs.length; j++) {
        input = inputs[j];
        input.className = "crosstalk_checkbox";
        input.nextElementSibling.className = "checkbox_label";
        var parent = input.parentNode;
        parent.className = (parent.className + " checkbox_container").trim();
        var checkmark = document.createElement('span');
        checkmark.className = "checkmark";
        // set element as child of input
        parent.insertBefore(checkmark, input.nextSibling);
      // input.checked = true;
        input.setAttribute("onchange", "changeFun('"+input.getAttribute("value")+"', this)");
        input.click();
      }
  }
}
  

// insert opengeoedu link where logo is
var logoNode = document.getElementsByClassName("navbar-logo")[0];
//alert(logoNode);
var ogoelink = document.createElement('a');
ogoelink.href = "https://www.opengeoedu.de";
ogoelink.target = "_blank";
ogoelink.title = "Projekt-Website";
// replace logo with link node
logoNode.parentNode.replaceChild(ogoelink, logoNode);
// make logo child of link (i.e. a clickable image)
ogoelink.appendChild(logoNode);

/*var sections = document.getElementsByClassName("section level1");
//var sections = document.getElementsByTagName("div");
alert("Found "+sections.length+" sections");

for (i = 0; i < sections.length; i++) {
    var section = sections[i];
    var footer = document.createElement('div');
    footer.innerHTML = "<p>Test footer</p>";
//    footer.className = "section level1 vertical-layout-fill dashboard-row-orientation";
//footer.className = "section level1 vertical-layout-fill dashboard-row-orientation";
    section.parentElement.insertBefore(footer, sections.nextSibling);

}*/ 

var c_select = document.getElementById('country_select');
var scripts = c_select.getElementsByTagName('script');
for(i = 0; i < scripts.length; i++){
  if(scripts[i].getAttribute('data-for') == 'country_select'){
    var dataNode = scripts[i];
    dataNode.innerHTML =  document.getElementById('country_select_map').innerHTML;
  
  }
}

updateSelectTool = function(){
  mapcontrols  = document.getElementsByClassName("select-inactive-active");
  if(mapcontrols.length >0)
    mapcontrols[0].title = "Rechteckauswahl (aktiv auf Karte und Tabelle)";

  mapcontrols  = document.getElementsByClassName("select-active-active");
  if(mapcontrols.length >0)
    mapcontrols[0].title = "Auswahl aufheben (auf Karte und Tabelle)";

}
onRenderMap = function(){


  var mapcontrols = document.getElementsByClassName("leaflet-control-zoom-in");
  if( mapcontrols .length >0)
      mapcontrols[0].title = "Vergrößern";
      mapcontrols  = document.getElementsByClassName("leaflet-control-zoom-out");
  if(mapcontrols.length >0)
    mapcontrols[0].title = "Verkleinern";
  mapcontrols  = document.getElementsByClassName("leaflet-control-fullscreen-button");
  if(mapcontrols.length >0)
     mapcontrols[0].title = "Vollbild";
  mapcontrols  = document.getElementsByClassName("select-inactive-active");
  if(mapcontrols.length >0){
    mapcontrols[0].setAttribute("onClick","updateSelectTool()");
    updateSelectTool();
  }
  mapcontrols  = document.getElementsByClassName("unnamed-state-active");
  if(mapcontrols.length >0)
      mapcontrols[0].title = "Ansicht zurücksetzen (reset)";
  mapcontrols  = document.getElementsByClassName("search-button");
  if(mapcontrols.length >0)
      mapcontrols[0].title = "Suche nach Ort oder Name eines Portals";


 var clickables = document.getElementsByClassName("polygonShape");
 for (var index = 0; index < clickables.length; index++) {
   clickables[index].addEventListener("click", 
      function(){   
          var buttons = document.getElementsByClassName("leaflet-popup-close-button");
          for (var index = 0; index < buttons.length; index++) {
            buttons[index].click();
            
          };
        }
     );   
 }

     /* var mapicons = document.getElementsByClassName("leaflet-marker-icon");
     // alert(mapicons[1].getAttributeNode("height"));
     for (var i; i < mapicons.length; i++){
      mapicons[i].style.width = "10px";
      mapicons[i].style.height="10px";
     }*/
     /*mapicons[1].setAttribute("height", "500px");
     mapicons[1].setAttribute("width", "500px");
     alert(mapicons[1].getAttribute("height"));
      /*
      var markerIcons = document.getElementsByClassName("leaflet-marker-icon");
	//alert(markerIcons.length);
 for (i = 0; i < markerIcons.length; i++) {
 	var markerIcon = markerIcons[i];
//alert(markerIcon);
   markerIcon.style.width = "5px";
   markerIcon.style.height = "5px";   
 }
 alert("test"); */
};

/*
$.fn.dataTable.enum( [ 'international', 'national', 'regional','kommunal' ] );
$('#portals_table').DataTable();
*/

$.fn.dataTable.ext.type.order['range-order-pre'] = function ( d ) {
  switch ( d ) {
      case 'international':    return 1;
      case 'national': return 2;
      case 'regional':   return 3;
      case 'kommunal':   return 4;
  }
  return 0;
};


//$(document).on('click', function (e) {
  /*if ($(e.target).closest("#CONTAINER").length === 0) {
      $("#CONTAINER").hide();
  }*/


  //alert("HI!!");
//});


  /*var buttons = document.getElementsByClassName("leaflet-popup-close-button");
  for (var index = 0; index < buttons.length; index++) {
     buttons[index].click();
    
  }*/
//  $(".leaflet-popup-close-button")[0].click();


// @license-end