var websocket;

$(document).ready(init);

function init() {
    var wsHost = "ws://" + window.location.host + "/ws";
    if(!("WebSocket" in window)){
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
        connect(wsHost);
    };
    $("#connected").hide();
    $("#content").hide();
};

function connect(wsHost)
{
    websocket = new WebSocket(wsHost);
    showScreen('<b>Connecting to: ' +  wsHost + '</b>');
    websocket.onopen = function(evt) { onOpen(evt) };
    websocket.onclose = function(evt) { onClose(evt) };
    websocket.onmessage = function(evt) { onMessage(evt) };
    websocket.onerror = function(evt) { onError(evt) };
};

function disconnect() {
    websocket.close();
};


function onOpen(evt) {
    showCurrentStatus('<span style="color: green;">CONNECTED </span>');
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
    var gameId =getParameterByName('id');
    websocket.send(gameId);
};

function onClose(evt) {
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function onMessage(evt) {
    showScreen('<span>' + evt.data+ '</span>');
};

function onError(evt) {
    showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
};

function showScreen(txt) {
    $('#output').html('<p>' + txt + '</p>');
};

function showCurrentStatus(txt) {
    $('#conn-status').html('<p>' + txt + '</p>');
};

function clearScreen()
{
    $('#output').html("");
};

// Get from http://stackoverflow.com/questions/901115/how-can-i-get-query-string-values-in-javascript
function getParameterByName(name) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results == null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
};
