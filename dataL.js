//Taken from FRISS Shiny.js Tutorials  4/30/2018
//https://shiny.rstudio.com/articles/js-send-message.html
// the event handler listens to shiny for messages send by handler1  
// if it receives a message, call the callback function doAwesomething and pass the message
Shiny.addCustomMessageHandler("handler1", outputDataLayer);

// this function is called by the handler, which passes the message
function outputDataLayer(message){
  message;
}