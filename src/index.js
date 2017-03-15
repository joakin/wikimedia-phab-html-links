if (process.env.NODE_ENV === 'production') {
  require('./rollbar');
}

require('./main.css');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

Elm.Main.embed(root, {text: getParameterByName('t') || ''});

function getParameterByName(name) {
  var match = RegExp('[?&]' + name + '=([^&]*)').exec(window.location.search)
  return match && decodeURIComponent(match[1].replace(/\+/g, ' '))
}
