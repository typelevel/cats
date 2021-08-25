/**
 * Toggle an specific class to the received DOM element.
 * @param {string}	elemSelector The query selector specifying the target element.
 * @param {string}	[activeClass='active'] The class to be applied/removed.
 */
function toggleClass(elemSelector, activeClass = "active") {
  const elem = document.querySelector(elemSelector);
  if (elem) {
    elem.classList.toggle(activeClass);
  }
}

/**
 * Toggle specific classes to an array of corresponding DOM elements.
 * @param {Array<string>}	elemSelectors The query selectors specifying the target elements.
 * @param {Array<string>}	activeClasses The classes to be applied/removed.
 */
function toggleClasses(elemSelectors, activeClasses) {
  elemSelectors.map((elemSelector, idx) => {
    toggleClass(elemSelector, activeClasses[idx]);
  });
}

/**
 * Remove active class from siblings DOM elements and apply it to event target.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function activate(element, activeClass = "active") {
  [...element.parentNode.children].map(elem =>
    elem.classList.remove(activeClass)
  );
  element.classList.add(activeClass);
}

/**
 * Remove active class from siblings parent DOM elements and apply it to element target parent.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function activateParent(element, activeClass = "active") {
  const elemParent = element.parentNode;
  activate(elemParent, activeClass);
}

/**
 * Remove active class from siblings parent DOM elements and apply it to element target parent.
 * @param {Element}		element The element receiving the class, and whose siblings will lose it.
 * @param {string}		[activeClass='active'] The class to be applied.
 */
function toggleParent(element, activeClass = "active") {
  const elemParent = element.parentNode;
  if (elemParent) {
    elemParent.classList.toggle(activeClass);
  }
}

/**
 * This will make the specified elements click event to show/hide the menu sidebar.
 */
function activateToggle() {
  const menuToggles = document.querySelectorAll("#menu-toggle, #main-toggle");
  if (menuToggles) {
    [...menuToggles].map(elem => {
      elem.onclick = e => {
        e.preventDefault();
        toggleClass("#wrapper", "toggled");
      };
    });
  }
}

/**
 * This will make the specified elements click event to behave as a menu
 * parent entry, or a link, or sometimes both, depending on the context.
 */
function activateMenuNesting() {
  const menuParents = document.querySelectorAll(".drop-nested");
  if (menuParents) {
    [...menuParents].map(elem => {
      elem.onclick = e => {
        e.preventDefault();
        toggleParent(elem, "open");
        const elementType = e.currentTarget.tagName.toLowerCase();
        if (elementType === "a") {
          const linkElement = e.currentTarget;
          const linkElementParent = linkElement.parentNode;
          const destination = linkElement.href;
          if (
            destination !== window.location.href &&
            !linkElementParent.classList.contains("active")
          ) {
            window.location.href = destination;
          }
        }
      };
    });
  }
}

/**
 * Aux function to retrieve repository stars and watchers count info from
 * GitHub API and set it on its proper nodes.
 */
async function loadGitHubStats() {
  const content = document.querySelector("#content");
  const ghOwner = content.dataset.githubOwner;
  const ghRepo = content.dataset.githubRepo;

  if (ghOwner && ghRepo) {
    const ghAPI = `https://api.github.com/repos/${ghOwner}/${ghRepo}`;
    const ghDataResponse = await fetch(ghAPI);
    const ghData = await ghDataResponse.json();
    const watchersElement = document.querySelector("#eyes");
    const starsElement = document.querySelector("#stars");
    watchersElement.textContent = ghData.subscribers_count;
    starsElement.textContent = ghData.stargazers_count;
  }
}

/**
 * Function to create an anchor with an specific id
 * @param {string}    id The corresponding id from which the href will be created.
 * @returns {Element} The new created anchor.
 */
function anchorForId(id) {
  const anchor = document.createElement("a");
  anchor.className = "header-link";
  anchor.href = `#${id}`;
  anchor.innerHTML = '<i class="fa fa-link"></i>';
  return anchor;
}

/**
 * Aux function to retrieve repository stars and watchers count info from
 * @param {string}	level The specific level to select header from.
 * @param {Element}	containingElement The element receiving the anchor.
 */
function linkifyAnchors(level, containingElement) {
  const headers = containingElement.getElementsByTagName(`h${level}`);
  [...headers].map(header => {
    if (typeof header.id !== "undefined" && header.id !== "") {
      header.append(anchorForId(header.id));
    }
  });
}

/**
 * Function
 */
function linkifyAllLevels() {
  const content = document.querySelector("#content");
  [...Array(7).keys()].map(level => {
    linkifyAnchors(level, content);
  });
}

window.addEventListener("DOMContentLoaded", () => {
  activateToggle();
  activateMenuNesting();
  loadGitHubStats();
  linkifyAllLevels();
});
