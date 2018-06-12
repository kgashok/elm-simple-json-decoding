/*global localStorage */
/*global $ */
var model = getModel();
var app = Elm.Fcc.fullscreen(model);

/*requirejs.config({
    "baseUrl": "js/lib",
    "paths": {
      "app": "../app",
      "jquery": "//ajax.googleapis.com/ajax/libs/jquery/2.0.0/jquery.min"
    }
});*/

//var $ = require('jquery');

/**
 * on elm's modelChange save the model to localStorage
 * @param  {object} model
 */

app.ports.modelChange.subscribe(function(model) {
  console.log ("Writing to storage..." + model); 
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


app.ports.popover.subscribe(function(val) {
  console.log ("popHover enabled"); 
  
  var originalLeave = $.fn.popover.Constructor.prototype.leave;

  $.fn.popover.Constructor.prototype.leave = function(obj){
    var self = obj instanceof this.constructor ?
      obj : $(obj.currentTarget)[this.type](this.getDelegateOptions()).data('bs.' + this.type)
    var container, timeout;
  
    originalLeave.call(this, obj);
  
    if(obj.currentTarget) {
      container = $(obj.currentTarget).siblings('.popover')
      timeout = self.timeout;
      container.one('mouseenter', function(){
        //We entered the actual popover – call off the dogs
        clearTimeout(timeout);
        //Let's monitor popover content instead
        container.one('mouseleave', function(){
          $.fn.popover.Constructor.prototype.leave.call(self, self);
        });
      })
    }
  };
  
  $('body').popover({ selector: '[data-popover]', trigger: 'click hover', placement: 'auto', delay: {show: 50, hide: 400}});
  
});




