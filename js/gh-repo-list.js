$(function() {
    var limit = 15;       // how many repos to list
    var login = 'wfarr'; // your username

    var placeHolderNode = document.getElementById("repos");
    var parentNode = placeHolderNode.parentNode;
    var insertedNode = document.createElement("ul");
    insertedNode.setAttribute("id", "repolist");
    parentNode.insertBefore(insertedNode, placeHolderNode.nextSibling);
    parentNode.removeChild(placeHolderNode);

  $.getJSON('http://github.com/api/v1/json/' + login + '?callback=?', function(data) {
    var repos = $.grep(data.user.repositories, function() {
                         return !this.fork;
                       });

    repos.sort(function(a, b) {
                 return b.watchers - a.watchers;
               });

    $.each(repos.slice(0, limit), function() {
             $('#repolist').append('<li><a href="' + this.url + '">' + this.name + '</a></li>');
           });
            });
  });