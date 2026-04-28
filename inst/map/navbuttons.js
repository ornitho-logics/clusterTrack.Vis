(function () {
  function initNavButtons() {
    var navs = document.querySelectorAll(".ct-navbuttons");

    navs.forEach(function (nav) {
      var prev = nav.dataset.prevMap || "";
      var next = nav.dataset.nextMap || "";

      var prevButton = nav.querySelector(".ct-navbutton-prev");
      var nextButton = nav.querySelector(".ct-navbutton-next");

      function configure(button, target) {
        if (!button) {
          return;
        }

        if (!target) {
          button.classList.add("ct-navbutton-disabled");
          return;
        }

        button.classList.remove("ct-navbutton-disabled");

        button.addEventListener("click", function () {
          window.location.href = target;
        });
      }

      configure(prevButton, prev);
      configure(nextButton, next);
    });
  }

  document.addEventListener("DOMContentLoaded", initNavButtons);

  document.addEventListener("keydown", function (e) {
    var tag = document.activeElement && document.activeElement.tagName;
    var typing = tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT";

    if (typing || e.altKey || e.ctrlKey || e.metaKey || e.shiftKey) {
      return;
    }

    var nav = document.querySelector(".ct-navbuttons");

    if (!nav) {
      return;
    }

    if (e.key === "ArrowLeft" && nav.dataset.prevMap) {
      e.preventDefault();
      window.location.href = nav.dataset.prevMap;
    }

    if (e.key === "ArrowRight" && nav.dataset.nextMap) {
      e.preventDefault();
      window.location.href = nav.dataset.nextMap;
    }
  });
})();