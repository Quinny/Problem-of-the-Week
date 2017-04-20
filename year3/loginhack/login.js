function pair(first, second) {
  return {
    first: first,
    second: second
  }
}

function zip(seq1, seq2) {
  var result = [];
  for (var i = 0; i < seq1.length && i < seq2.length; ++i) {
    result.push(pair(seq1[i], seq2[i]));
  }
  return result;
}

function convolutedEquals(s1, s2) {
  var f = p => p.first == p.second;
  return s1.length == s2.length && zip(s1, s2).every(f);
}

function validate(creds) {
  return convolutedEquals($("#username").val(), creds.username)
    && convolutedEquals($("#password").val(), creds.password);
}

function alertIfGood(creds) {
  if (validate(creds)) alert("You got it!");
  else alert("Nope!");
}

$(document).ready(() => {
  $("#button").on("click", () => {
    $.getJSON("RU37GzrX4C.json", alertIfGood);
  });
});
