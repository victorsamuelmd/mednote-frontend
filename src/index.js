import {
    Main
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Main.embed( document.getElementById( 'root' ) );

registerServiceWorker();

function parseJwt (token) {
    var base64Url = token.split('.')[1];
    var base64 = base64Url.replace('-', '+').replace('_', '/');
    return JSON.parse(window.atob(base64));
};

app.ports.toJs.subscribe(function (str) {
    switch(str){
        case "session":
            var token = localStorage.getItem("token");
            if (token) {
                var session = parseJwt(token);
                var obj = {
                    username: session.username,
                    grupo: session.grupo,
                    authorization: token
                }
                app.ports.getSession.send(obj);
            }
            break;
        case "salir":
            localStorage.clear()
            break;
    }
});

app.ports.showPicker.subscribe(function (str) {
    var picker = $('#datepicker').datepicker({
        uiLibrary: 'bootstrap4',
        modal: true,
        format: 'yyyy/mm/dd'
    });
    picker.change(function () {
        app.ports.toElm.send(picker.val());
    });
    
});

app.ports.storeSession.subscribe(function (user) {
   localStorage.setItem("token", user) 
});



