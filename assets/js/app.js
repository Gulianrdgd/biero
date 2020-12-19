import "../css/app.scss"
import "phoenix_html"
import { Elm } from "../src/Main.elm";

var main = Elm.Main.init({
    node: document.getElementById('elm-main'),
    flags : {url: window.location.href }
});

main.ports.sendMessage.subscribe(function(message) {
    if (message.includes("?username:")){
    sessionStorage.setItem("username", message.substr(10));
    }
    else if(message.includes("?isHost:")) {
        sessionStorage.setItem("isHost", message.charAt(message.length - 1));
    }
    else {
        console.log(message);
    }
});