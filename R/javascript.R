# Cookie handling
.jsCode <- '
  shinyjs.init = function() {
    shinyjs.getcookies();
  };
  
  shinyjs.pageCol = function(params) {
    $("body").css("background", params);
    shinyjs.setcookie({name: "colour", value: params})
  }
  
  shinyjs.getcookies = function(params) {
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }

  shinyjs.setcookie = function(params) {
    Cookies.set(params.name, escape(params.value));  
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }

  shinyjs.rmcookie = function(name) {
    Cookies.remove(name);
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"});
  }
  
  shinyjs.rmcookies = function(params) {
    for (var key in Cookies.get()) {
      Cookies.remove(key);
    }
    Shiny.setInputValue("jscookie", Cookies.get(), {priority: "event"})
  }
'

.jsCodeFunctions <- c('getcookies', 'setcookie', 'rmcookie', 'rmcookies', 'pageCol')