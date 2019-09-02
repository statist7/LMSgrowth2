# Cookie handling
.jsCode <- '
  shinyjs.init = function() {
    $(document).on("shiny:sessioninitialized", function(event) {
      shinyjs.getcookies();
    });
  };
  
  // NOTE: namespace prefix "globals-" targets inputs in the globals module
  
  shinyjs.getcookies = function(params) {
    Shiny.setInputValue("globals-jscookie", Cookies.get());
  }

  shinyjs.setcookie = function(params) {
    Cookies.set(params.name, escape(params.value), { expires: 365 });  
    Shiny.setInputValue("globals-jscookie", Cookies.get());
  }

  shinyjs.rmcookie = function(name) {
    Cookies.remove(name);
    Shiny.setInputValue("globals-jscookie", Cookies.get());
  }
  
  shinyjs.rmcookies = function(params) {
    for (var key in Cookies.get()) {
      Cookies.remove(key);
    }
    Shiny.setInputValue("globals-jscookie", Cookies.get())
  }
'

.jsCodeFunctions <- c('getcookies', 'setcookie', 'rmcookie', 'rmcookies')