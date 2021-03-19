/*static version*/
var ws;
var adr;
var clog={};
var pingerid;

if (typeof clog_debug == 'undefined') {
    clog_debug = false;
}

function Ping_ws() {
    if (ws.readyState == 1) {
        ws.send ('0');
    }
}

function Shutdown_ws(event) {
    if (ws != null) {
	ws.onerror = null;
	ws.onclose = null;
	ws.close ();
	ws = null;
    }
    clearInterval (pingerid);
    if (clog['html_on_close'] != '') {
        $(document.body).html(clog['html_on_close']);
    }
}

function Setup_ws() {
    ws.onmessage = function (event) {
        try {
            if (clog_debug == true) {
		console.log ('eval data = ' + event.data);
            }
            eval (event.data);
        } catch (e) {
            console.error (e.message);
        }
    }
    
    ws.onerror = function (event) {
        console.log ('onerror: reconnect');
        ws = null;
        ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('onerror: reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('onerror: reconnect failure');
            Shutdown_ws(event);
        }
    }
    
    ws.onclose = function (event) {
        console.log ('onclose: reconnect');
        ws = null;
        ws = new WebSocket (adr  + '?r=' + clog['connection_id']);
        ws.onopen = function (event) {
            console.log ('onclose: reconnect successful');
            Setup_ws();
        }
        ws.onclose = function (event) {
            console.log ('onclose: reconnect failure');
            Shutdown_ws(event);
        }
    }
}

$( document ).ready(function() {
    var s = document.location.search;
    var tokens;
    var r = /[?&]?([^=]+)=([^&]*)/g;

    clog['body']=document.body;
    clog['head']=document.head;
    clog['documentElement']=document.documentElement;
    clog['window']=window;
    clog['navigator']=navigator;
    clog['document']=window.document;
    clog['location']=window.location;
    
    if (location.protocol == 'https:') {
        adr = 'wss://' + location.hostname;
    } else {
        adr = 'ws://' + location.hostname;
    }
    
    if (location.port != '') { adr = adr + ':' + location.port; }
    adr = adr + '/clog';
    
    try {
        console.log ('connecting to ' + adr);
        ws = new WebSocket (adr);
    } catch (e) {
        console.log ('trying again, connecting to ' + adr);
        ws = new WebSocket (adr);
    }
    
    if (ws != null) {
        ws.onopen = function (event) {
            console.log ('connection successful');
            Setup_ws();
        }
        pingerid = setInterval (function () {Ping_ws ();}, 10000);
    } else {
        document.writeln ('If you are seeing this your browser or your connection to the internet is blocking websockets.');
    }
});
