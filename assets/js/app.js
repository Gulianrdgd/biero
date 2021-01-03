import "../css/app.scss"
import "phoenix_html"
import {Socket} from "phoenix"
import { Elm } from "../src/Main.elm";
import {getVal} from "./hexToStyle.js";

let socket = new Socket("/socket");
let channel = socket.channel('biero:front');
socket.connect();
channel.join();

const etappePos = [[135, 100], [500, 110], [920, 100], [1650, 80], [1460, 400], [830, 554], [183, 565], [540, 750], [1250, 860]];
const IMAGESIZE = [1920,1080];
const regeneratorRuntime = require("regenerator-runtime");


var main = Elm.Main.init({
    node: document.getElementById('elm-main'),
    flags : {url: window.location.protocol + "//" + window.location.host}
});


let payloadTemp;

channel.on('shout', payload => {
    switch (payload.body) {
        case "?newTable":
            toStringFunc(payload.table, payload.type);
            main.ports.messageReceiver.send(JSON.stringify(payload));
            payloadTemp = payload;
            break;
        default:
            main.ports.messageReceiver.send(JSON.stringify(payload));
            break;
    }
});

function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}
function findMatch(posLeft, posTop, posArr){
    for(let i = 0; i<posArr.length; i++){
        if(Math.abs(posArr[i][0] - posLeft) <= 10 && Math.abs(posArr[i][1] - posTop) <= 10){
            return false;
        }
    }
    return true;
}

function convertRemToPixels(rem) {
    return rem * parseFloat(getComputedStyle(document.documentElement).fontSize);
}

function toStringFunc(table, type){
    let pos = 0;

    let windowWidth = window.innerWidth * 0.8;
    let imageWidth = window.innerHeight/(IMAGESIZE[1]/IMAGESIZE[0]);
    if(imageWidth > windowWidth){
        imageWidth = windowWidth;
    }

    let worldLeft = (windowWidth - imageWidth)/2;

    let imageHeight = (windowWidth) * (IMAGESIZE[1]/IMAGESIZE[0]);
    if(imageHeight > window.innerHeight){
        imageHeight = window.innerHeight;
    }

    let worldTop = (window.innerHeight - imageHeight)/2;

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

        table[val].posLeft = (worldLeft +  ((imageWidth /IMAGESIZE[0]) * etappePos[table[val].etappe][0]) + pos).toString();
        table[val].posTop = (worldTop + ((imageHeight / IMAGESIZE[1]) * etappePos[table[val].etappe][1]) + pos).toString();
        pos+=window.innerWidth/150;
    }
}

function reportWindowSize() {
    toStringFunc(payloadTemp.table, "teams");
    main.ports.messageReceiver.send(JSON.stringify(payloadTemp));
}

//
// window.addEventListener('mouseup', e => {
//     console.log(e.x);
//     console.log(e.y);
//     let imageWidth = window.innerHeight/(IMAGESIZE[1]/IMAGESIZE[0]);
//     if(imageWidth > window.innerWidth){
//         imageWidth = window.innerWidth;
//     }
//
//     let worldLeft = window.innerWidth - imageWidth;
//
//     let imageHeight = window.innerWidth * (IMAGESIZE[1]/IMAGESIZE[0]);
//     if(imageHeight > window.innerHeight){
//         imageHeight = window.innerHeight;
//     }
//
//     let worldTop = window.innerHeight - imageHeight;
//     console.log("Width: " + imageWidth);
//     console.log("Heigth: " + imageHeight);
//     console.log("Offset left: " + worldLeft);
//     console.log("Offset top: " + worldTop);
// });

channel.push('shout', {username: "init",  body: "?getTable"});
window.onresize = reportWindowSize;