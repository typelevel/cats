
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

document.addEventListener('DOMContentLoaded', () => {
  initNavToggle();
  initTabs();
});
