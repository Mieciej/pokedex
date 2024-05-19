function imageLoaded(imageId) {
  console.log("Image loaded: " + imageId);
  // take the last character (digit) from the imageId
  var id = imageId[imageId.length - 1];
  var img = document.getElementById(imageId);
  var spinner = document.getElementById('spinner' + id);

  // delay by 2 seconds
  setTimeout(function() {
    spinner.style.display = 'none';
    img.style.display = 'block';
  }, 1000);
}