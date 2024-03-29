function get(url, f)
{
    console.log(url);
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
    var tabElt = document.getElementById("tab");
    if (tabElt) {
        var username = document.getElementById("username-join").value;
        if (tabElt.value == "create") {
            var username = document.getElementById("username-create").value;
        }
        if (username && !(username === "")) {
	    setCookie("username", username)
        }
    }
}

function setStartingTab() {
    var create = document.getElementById("createPage");
    var join = document.getElementById("joinPage");
    if (create && createTab) {
        if (document.getElementById("tab").value == "create") {
            create.style.display="block";
            join.style.display="none";
        } else {
            join.style.display="block";
            create.style.display="none";
        }
    }
}

function replaceContent (content) {
    var el = document.createElement( 'body' );
    el.innerHTML = content;		
    if (document.getElementById("game").innerHTML == el.innerHTML) {
	
    } else {
	document.getElementById("game").innerHTML = content;
    }
    var refresh = document.getElementById("refreshpage");
    if (refresh) {
	setTimeout(getUpdatedPage, refresh.innerHTML)
    }
    setStartingTab()
}


function getUpdatedPage () {
    var username = getCookie("username")
    var url = "page"
    if (username) {
	var url = "page?username=" + username
    }
    get(url, replaceContent)
}

function usernameQuery () {
    var username = getCookie("username");
    return ("username=" + username)
}

function createGame () {
    updateUsername();
    get("action/create?" + usernameQuery(), getUpdatedPage);
    get("action/join?" + usernameQuery(), getUpdatedPage);
}

function joinGame () {
    updateUsername();
    var code = document.getElementById("code").value;
    code = code.toUpperCase();
    get("action/join?" + usernameQuery() + "&code=" + code, getUpdatedPage);
}
    
function startGame () {
    get("action/start_game?" + usernameQuery(), getUpdatedPage);
}

function newGame () {
    get("action/new_game?" + usernameQuery(), getUpdatedPage);
}

function ack () {
    get("action/input/ack?" + usernameQuery(), getUpdatedPage);
} 

function vote () {
    get("action/input/vote?" + usernameQuery(), getUpdatedPage);
} 

function doNothing (str) { }

function changeNumberedRole (role) {
    var count = document.getElementById(role).value;
    if (!(isNaN(count))) {
	get("action/set_role?role=" + role + "&count=" + count + "&" + usernameQuery(), doNothing);
    }
} 

function changeSingleRole (role) {
    var role_checked = document.getElementById(role).checked;
    var count = role_checked ? "1" : "0";
    get("action/set_role?role=" + role + "&count=" + count + "&" + usernameQuery(), doNothing);
}    

function getSelectedUsers () {
    var items = document.getElementsByName("users");
    var selectedUsers = [];
    if (items) {
	for (var i = 0; i < items.length; i++) {
	    var item = items[i];
	    if (item.checked) {
		selectedUsers[selectedUsers.length] = item.value
	    }
	}
    }
    return selectedUsers;
}

function chooseUsers () {
    var users = getSelectedUsers ();
    if (users) {
	var userParam = users.join(",")
	get("action/input/choose_user?users=" + userParam + "&" + usernameQuery (), 
	    getUpdatedPage);
    }
}

function endGame() {
    get("action/end_game?" + usernameQuery (), getUpdatedPage);
}

function leaveGame() {
    get("action/leave?" + usernameQuery (), getUpdatedPage);
}

function toSettings () {
    get("action/to_settings?" + usernameQuery (), getUpdatedPage)
}

function toSetup () {
    get("action/to_setup?" + usernameQuery (), getUpdatedPage)
}

function joinTab () {
    var tab = document.getElementById("tab")
    if (tab) {
        tab.value = "join"
        setStartingTab()
    }
}

function createTab () {
    var tab = document.getElementById("tab")
    if (tab) {
        tab.value = "create"
        setStartingTab()
    }
}


function setSetting(setting) {
    var checked = document.getElementById(setting).checked;
    get("action/set_setting?" + usernameQuery () + "&" + "setting=" + setting + "&" + "is_on=" + checked, getUpdatedPage)
}

function incrInput(role) {
    var count = document.getElementById(role).value;
    var count = parseInt(count);
    if ((role === "Alpha Wolf" || role === "Doppelganger") && count >= 1) {
        return 
    }
    document.getElementById(role).value = parseInt(count) + 1;
    changeNumberedRole(role)
}

function decrInput(role) {
    console.log(role);
    var count = document.getElementById(role).value;
    count = parseInt(count)
    if (count > 0) {
        document.getElementById(role).value = count - 1;
        changeNumberedRole(role)
    }
}

getUpdatedPage();
