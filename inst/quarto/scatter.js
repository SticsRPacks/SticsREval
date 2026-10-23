// Scatter plots are collapsible `<details>`: open the one targeted by a
// link (e.g. a variable badge, or a cell of the homepage heatmap), whether
// it comes from another page (URL hash) or from this one (click).
function openScatter(id) {
  var el = document.getElementById(decodeURIComponent(id));
  if (el && el.tagName === "DETAILS") {
    el.open = true;
    el.scrollIntoView();
  }
}
document.addEventListener("DOMContentLoaded", function() {
  if (location.hash) openScatter(location.hash.slice(1));
});
window.addEventListener("hashchange", function() {
  openScatter(location.hash.slice(1));
});
document.addEventListener("click", function(event) {
  var link = event.target.closest('a[href^="#scatter-"]');
  if (link) openScatter(link.getAttribute("href").slice(1));
});
// A plot drawn in a closed block has no width: resize the plots (htmlwidgets
// listens to window resizes) when a block is opened. `toggle` doesn't
// bubble, hence the capture.
document.addEventListener("toggle", function(event) {
  if (event.target.matches("details.scatter") && event.target.open) {
    window.dispatchEvent(new Event("resize"));
  }
}, true);
function toggleScatters(open) {
  document.querySelectorAll("details.scatter").forEach(function(el) {
    el.open = open;
  });
}
