document.addEventListener('DOMContentLoaded', function() {
  var codeChunks = document.querySelectorAll('code.language-r');
  codeChunks.forEach(function(chunk) {
    chunk.classList.add('code-r-chunk');
  });
});
