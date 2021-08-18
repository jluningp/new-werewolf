function get(url, f)
{
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
            f (xmlHttp.responseText);
    }
    xmlHttp.open("GET", url, true); 
    xmlHttp.send(null);
} 

function getCookie(name) {
  var matches = document.cookie.match(new RegExp(
    "(?:^|; )" + name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"
  ));
  return matches ? decodeURIComponent(matches[1]) : undefined;
}

function setCookie(name, value, options = {}) {
  if (options.expires instanceof Date) {
    options.expires = options.expires.toUTCString();
  }

  var updatedCookie = encodeURIComponent(name) + "=" + encodeURIComponent(value);

  for (var optionKey in options) {
    updatedCookie += "; " + optionKey;
    var optionValue = options[optionKey];
    if (optionValue !== true) {
      updatedCookie += "=" + optionValue;
    }
  }
  document.cookie = updatedCookie;
}

function updateUsername () {
    var username = document.getElementBy_id("username").value;
    if (!(username === "")) {
	setCookie("username", username)
    }
}



var username = getCookie("username")
var url = ""
if (username) {
    var url = "?username=" + username
}


function replaceContent (content) {
    document.open();
    document.write(content);
    document.close()
}

window.setInterval (function () { get(url, replaceContent)}, 1000) 


