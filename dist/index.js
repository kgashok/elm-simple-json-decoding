var model = getModel();
var app = Elm.Fcc.fullscreen(model);

/**
 * on elm's modelChange save the model to localStorage
 * @param  {object} model
 */

app.ports.modelChange.subscribe(function(model) {
  localStorage.setItem('fccModel', JSON.stringify(model));
});


app.ports.logExternalOut.subscribe(function (value) {
  console.info("logs:", value);
});

/**
 * getModel
 * Return a parsed model or null
 */

function getModel () {
  var model = localStorage.getItem('fccModel');
  return model ? JSON.parse(model) : null;
}
