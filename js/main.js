jQuery(document).ready(function() {
  hljs.initHighlightingOnLoad();
  activeToggle();
  loadGitHubStats();
});


function activeToggle() {
  $("#menu-toggle").click(function(e) {
    e.preventDefault();
    $("#wrapper").toggleClass("toggled");
  });
}


var baseURL = window.location.href;

function shareSiteFacebook(text) {
  launchPopup('http://www.facebook.com/sharer/sharer.php?u='+baseURL+'&t=' + text);
}

function shareSiteTwitter(text) {
  launchPopup('https://twitter.com/home?status=' + text);
  return false;
}

function shareSiteGoogle() {
  launchPopup('https://plus.google.com/share?url='+baseURL);
  return false;
}

function launchPopup(url) {
  window.open(url, 'Social Share', 'height=320, width=640, toolbar=no, menubar=no, scrollbars=no, resizable=no, location=no, directories=no, status=no');
}

function loadGitHubStats() {
  var content = $("#content");
  var githubOwner = content.attr("data-github-owner")
  var githubRepo = content.attr("data-github-repo")

  if(githubOwner && githubRepo) {
    var gitHubAPI = "https://api.github.com/repos/" + githubOwner + "/" + githubRepo + "?callback=?";
    $.getJSON(gitHubAPI).done(function(data) {
      $('#eyes').text(data.data.subscribers_count);
      $('#stars').text(data.data.stargazers_count);
    });
  }

}