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
    var username = document.getElementById("username").value;
    if (!(username === "")) {
	setCookie("username", username)
    }
}

function replaceContent (content) {
    document.getElementById("game").innerHTML = content;
    var refresh = document.getElementById("refreshpage");
    if (refresh) {
	setTimeout(getUpdatedPage, refresh.innerHTML)
    }
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
}

function joinGame () {
    updateUsername();
    get("action/join?" + usernameQuery(), getUpdatedPage);
}
    
function startGame () {
    get("action/start_game?" + usernameQuery(), getUpdatedPage);
}

function ack () {
    get("action/input/ack?" + usernameQuery(), getUpdatedPage);
} 

function changeWerewolf () {
    var werewolves = document.getElementById("werewolves").value;
    if (!(isNaN(werewolves))) { 
	get("action/set_role?role=Werewolf&count=" + werewolves + "&" + usernameQuery(), getUpdatedPage);
    }
} 


    
function changeRobber () {
    var robber = document.getElementById("robber").checked;
    var count = robber ? "1" : "0";
    get("action/set_role?role=Robber&count=" + count + "&" + usernameQuery(), getUpdatedPage);
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


getUpdatedPage();