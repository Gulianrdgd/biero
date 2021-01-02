import "../css/app.scss"
import "phoenix_html"
import {Socket} from "phoenix"
import { Elm } from "../src/Main.elm";
import {getVal} from "./hexToStyle.js";

let socket = new Socket("/socket");
let channel = socket.channel('biero:front');
socket.connect();
channel.join();

const etappePos = [[192, 168], [594, 168], [1007, 170], [1517, 456], [830, 554], [183, 565], [638, 938], [1355, 893]];
const regeneratorRuntime = require("regenerator-runtime");

var main = Elm.Main.init({
    node: document.getElementById('elm-main'),
    flags : {url: window.location.protocol + "//" + window.location.host}
});

channel.on('shout', payload => {
    switch (payload.body) {
        case "?newTable":
            toStringFunc(payload.table, payload.type);
            console.log(payload.table);
            main.ports.messageReceiver.send(JSON.stringify(payload));
            break;
        default:
            main.ports.messageReceiver.send(JSON.stringify(payload));
            break;
    }
});

function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function toStringFunc(table, type){
    for(let val in table) {
        if(type === "Users") {
            if (table[val].hasAdmin) {
                table[val].hasAdmin = "T";
            } else {
                table[val].hasAdmin = "F";
            }
        }else{
            table[val].etappe = table[val].etappe.toString();
            table[val].users = table[val].users.toString();
        }
        table[val].cssColor = getVal(table[val].color);
        table[val].posLeft = ((window.innerWidth/1920) * etappePos[table[val].etappe][0] + getRandomInt(window.innerWidth/100)).toString();
        table[val].posTop = ((window.innerHeight/1080) * etappePos[table[val].etappe][1] + getRandomInt(window.innerHeight/50)).toString();
    }
}

channel.push('shout', {username: "init",  body: "?getTable"});