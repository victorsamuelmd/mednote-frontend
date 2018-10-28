import {
  Elm
} from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

function parseJwt(token) {
  var base64Url = token.split('.')[1];
  var base64 = base64Url.replace('-', '+').replace('_', '/');
  return JSON.parse(window.atob(base64));
};

function getSession(tok) {
  if (tok !== null) {
    var session = parseJwt(tok);
    return {
      usuario: session.usuario,
      grupo: session.grupo,
      autorizacion: tok,
      id: session.id
    }
  }
  return null
};

var token = localStorage.getItem("token");
var obj = getSession(token);

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: obj
});

registerServiceWorker();
/*
app.ports.toJs.subscribe(function(str) {
  switch (str) {
    case "salir":
      localStorage.clear()
      break;
  }
});

*/

app.ports.storeSession.subscribe(function(user) {
  localStorage.setItem("token", user)
  var newToken = localStorage.getItem("token")
  app.ports.gotSession.send(getSession(newToken));
});
