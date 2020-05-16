shinyjs.init = function() {
  $(document).on("shiny:sessioninitialized", function(event) {
    shinyjs.getcookies();
  });
};

// NOTE: namespace prefix "preferences-" targets inputs in the preferences module

shinyjs.getcookies = function(params) {
  Shiny.setInputValue("preferences-jscookie", Cookies.get());
};

shinyjs.setcookie = function(params) {
  Cookies.set(params.name, escape(params.value), { expires: 365 });  
  Shiny.setInputValue("preferences-jscookie", Cookies.get());
};

shinyjs.rmcookie = function(name) {
  Cookies.remove(name);
  Shiny.setInputValue("preferences-jscookie", Cookies.get());
};

shinyjs.rmcookies = function(params) {
  for (var key in Cookies.get()) {
    Cookies.remove(key);
  }
  Shiny.setInputValue("preferences-jscookie", Cookies.get());
};
