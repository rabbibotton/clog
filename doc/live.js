function mglpaxAddDocumentListener() {
  const paxToDocument = document.getElementById('paxToDocument');
  paxToDocument.addEventListener('keypress', function(event) {
    if (event.key === 'Enter') {
      const inputText = paxToDocument.value;
      const url = new URL(window.location.href);
      const baseUrl = url.origin +
        url.pathname.substring(0, url.pathname.lastIndexOf('/') + 1);
      window.location.href = baseUrl + "pax:" + inputText + url.search;
    }
  });
}

function mglpaxAddAproposListener() {
  const paxToApropos = document.getElementById('paxToApropos');
  paxToApropos.addEventListener('keypress', function(event) {
    if (event.key === 'Enter') {
      const inputText = paxToApropos.value;
      const url = new URL(window.location.href);
      const baseUrl = url.origin +
        url.pathname.substring(0, url.pathname.lastIndexOf('/') + 1);
      window.location.href = baseUrl + "pax-eval:" + 
        '(mgl-pax::pax-apropos* ' +
        encodeURIComponent(toLispLiteralString(inputText)) +
        ' t nil nil)' + url.search;
    }
  });
}

function toLispLiteralString(str) {
  let literal = '"'; // Start with an opening double quote
  for (let i = 0; i < str.length; i++) {
    const char = str[i];
    switch (char) {
      case '"':
        literal += '\\"';
        break;
      case '\\':
        literal += '\\\\';
        break;
      default:
        literal += char;
    }
  }
  literal += '"'; // End with a closing double quote
  return literal;
}
