import "../css/app.scss"
import {Socket} from "phoenix"
import "phoenix_html"
import { Elm } from "../src/Admin.elm";

const regeneratorRuntime = require("regenerator-runtime");

let token =  sessionStorage.getItem("token");
let username = sessionStorage.getItem("username");
let socket = new Socket("/socket", {params: {username: username, token: token}});
let channel = socket.channel('biero:admin', {username: username, token: token});


socket.connect();
socket.onError(function x(){
    socket.disconnect();
    window.location = "/";
})


channel.join();

var main = Elm.Admin.init({
    node: document.getElementById('elm-admin'),
    flags : {url: window.location.protocol + "//" + window.location.host}
});

//////// Init data Elm /////////
main.ports.messageReceiver.send(JSON.stringify({"?username": username}));
main.ports.messageReceiver.send(JSON.stringify({"?token": token}));

//////// End init data Elm /////////

function toStringFunc(table, type){
    for(let val in table) {
        if(type === "Users") {
            if (table[val].hasAdmin) {
                table[val].hasAdmin = "T"
            } else {
                table[val].hasAdmin = "F"
            }
        }else{
            table[val].etappe = table[val].etappe.toString();
            table[val].users = table[val].users.toString();
        }
    }
}

main.ports.sendMessage.subscribe(async function(payload) {
    let message = JSON.parse(payload)
    console.log(payload);
    console.log(message);
    switch (message.message){
        case "?getTable":
            channel.push('shout', {username: username,  body: "?getTable", token: message.token, table: message.table});
            break;
        case "?sendChanges":
            channel.push('shout', {username: username,  body: "?sendChanges", token: message.token, newUsers: message.newUsers, newTeams: message.newTeams});
            break;
        default:
            break;
    }
});

channel.on('shout', payload => {
    console.log(payload);
    switch (payload.body) {
        case "?newTable":
            toStringFunc(payload.table, payload.type);
            main.ports.messageReceiver.send(JSON.stringify(payload));
            break;
        default:
            main.ports.messageReceiver.send(JSON.stringify(payload));
            break;
    }
});
