jQuery(document).ready(function() {
  hljs.initHighlightingOnLoad();
  activeToggle();
  loadGitHubStats();
  linkifyAllLevels(".docs .content-wrapper");
});


function activeToggle() {
  $("#menu-toggle").click(function(e) {
    e.preventDefault();
    $("#wrapper").toggleClass("toggled");
  });
}

var anchorForId = function (id) {
  var anchor = document.createElement("a");
  anchor.className = "header-link";
  anchor.href      = "#" + id;
  anchor.innerHTML = "<i class=\"fa fa-link\"></i>";
  return anchor;
};

var linkifyAnchors = function (level, containingElement) {
  var headers = containingElement.getElementsByTagName("h" + level);
  for (var h = 0; h < headers.length; h++) {
    var header = headers[h];

    if (typeof header.id !== "undefined" && header.id !== "") {
      header.appendChild(anchorForId(header.id));
    }
  }
};

var linkifyAllLevels = function (blockSelector) {
  var contentBlock = document.querySelector(blockSelector);
  if (!contentBlock) {
    return;
  }
  for (var level = 1; level <= 6; level++) {
    linkifyAnchors(level, contentBlock);
  }
};

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
