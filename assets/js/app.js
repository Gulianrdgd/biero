import "../css/app.scss"
import "phoenix_html"
import {Socket} from "phoenix"
import { Elm } from "../src/Main.elm";
import {getVal} from "./hexToStyle.js";

let socket = new Socket("/socket");
let channel = socket.channel('biero:front');
socket.connect();
channel.join();

const etappePos = [[331, 246], [267, 573], [487, 887], [902, 647], [693, 378], [1009, 178], [1604, 246], [1275, 491], [1590, 783], [1129, 903]];
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


function toStringFunc(table, type){
    let pos = 0;
    let etappe = -1;
    // Bootstrap col spacing
    let windowWidth;
    if(window.innerWidth < 768){ // SMOL
        windowWidth = window.innerWidth;
    }else if(window.innerWidth >= 1200){ // XL
        windowWidth = window.innerWidth * (1/12*10);
    } else {
        windowWidth = window.innerWidth * (1/12*8);
    }
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

    table.sort(function (a,b){
        if (a.etappe > b.etappe){
            return -1
        }else if(a.etappe < b.etappe){
            return 1;
        }else{
            return 0;
        }
    });

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

        if(etappe === table[val].etappe) {
            pos += window.innerWidth / 150;
        } else{
            etappe = table[val].etappe;
            pos = 0;
        }

        table[val].posLeft = (worldLeft +  ((imageWidth /IMAGESIZE[0]) * etappePos[table[val].etappe][0]) + pos).toString();
        table[val].posTop = (worldTop + ((imageHeight / IMAGESIZE[1]) * etappePos[table[val].etappe][1]) + pos).toString();
    }
}

function reportWindowSize() {
    toStringFunc(payloadTemp.table, "teams");
    main.ports.messageReceiver.send(JSON.stringify(payloadTemp));
}


channel.push('shout', {username: "init",  body: "?getTable"});
window.onresize = reportWindowSize;
