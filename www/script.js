function imageLoaded(imageId) {
  var id = imageId[imageId.length - 1];
  var img = document.getElementById(imageId);
  var spinner = document.getElementById('spinner' + id);

  setTimeout(function() {
    spinner.style.display = 'none';
    img.style.display = 'block';
  }, 1000);
}

function imageLoadedComparisonDasboard(imageId) {
  var id = imageId[imageId.length - 2] + imageId[imageId.length - 1];
  var img = document.getElementById(imageId);
  var spinner = document.getElementById('spinner' + id);
  
  setTimeout(function() {
    spinner.style.display = 'none';
    img.style.display = 'block';
  }, 1000);
}