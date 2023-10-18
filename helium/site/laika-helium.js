
function switchTabs (elements, choiceName) { 
  elements.forEach(elem => {
    if (elem.dataset.choiceName === choiceName) elem.classList.add("active");
    else (elem.classList.remove("active"));
  });
}

function initTabs () {
  const groups = {};
  const tabs = document.querySelectorAll(".tab")
  const content = document.querySelectorAll(".tab-content")
  tabs.forEach(tab => {
    const groupName = tab.parentElement.parentElement.dataset.tabGroup;
    const choiceName = tab.dataset.choiceName;
    if (groupName && choiceName) {
      if (groups[groupName] === undefined) groups[groupName] = [];
      groups[groupName].push(tab);
      tab.firstElementChild.onclick = (e) => {
        e.preventDefault();
        switchTabs(groups[groupName], choiceName);
      };
    }
  });
  content.forEach(c => {
    const group = c.parentElement.dataset.tabGroup;
    if (group && groups[group]) groups[group].push(c);
  });
}

function initNavToggle () {
  const navIcon = document.getElementById("nav-icon");
  const sidebar = document.getElementById("sidebar");
  if (navIcon && sidebar) {
    navIcon.onclick = () => {
      sidebar.classList.toggle("nav-open");
    };
  }
}

function initMenuToggles () {
  // this functionality applies to all types of menus, including the version menu
  document.querySelectorAll(".menu-container").forEach((container) => {
    const toggle = container.querySelector(".menu-toggle");
    const content = container.querySelector(".menu-content");
    if (toggle && content) {
      const closeHandler = (evt) => {
        const contentClicked = evt.target.closest(".menu-content");
        const toggleClicked = evt.target.closest(".menu-toggle");
        if ((!toggleClicked || toggleClicked !== toggle) && (!contentClicked || contentClicked !== content)) {
          content.classList.remove("menu-open");
          document.removeEventListener("click", closeHandler)
        }
      }
      toggle.onclick = () => {
        if (content.classList.toggle("menu-open")) {
          document.addEventListener("click", closeHandler);
        }
      };
    }
  });
}

document.addEventListener('DOMContentLoaded', () => {
  initNavToggle();
  initMenuToggles();
  initTabs();
});
