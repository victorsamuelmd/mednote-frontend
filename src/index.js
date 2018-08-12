import {
    Main
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

function parseJwt (token) {
    var base64Url = token.split('.')[1];
    var base64 = base64Url.replace('-', '+').replace('_', '/');
    return JSON.parse(window.atob(base64));
};

var token = localStorage.getItem("token");
var obj;

if (token) {
    var session = parseJwt(token);
    obj = {
        username: session.username,
        grupo: session.grupo,
        authorization: token
    }
}

var app = Main.embed( document.getElementById( 'root' ), obj || null );

registerServiceWorker();

app.ports.toJs.subscribe(function (str) {
    switch(str){
        case "salir":
            localStorage.clear()
            break;
    }
});


app.ports.storeSession.subscribe(function (user) {
   localStorage.setItem("token", user) 
});



