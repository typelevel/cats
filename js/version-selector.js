/* When the user clicks on the navigation Documentation button,
 * toggle between hiding and showing the dropdown content.
 */
function displayToggle(e) {
  e.preventDefault();
  e.stopPropagation();
  // Calling close func. in case we're clicking another dropdown with one opened
  closeDropdown(e);
  const parent = e.target.closest("div[id$='version-dropdown']");
  if (parent) {
    const dropdown = parent.querySelector(".dropdown-content");
    if (dropdown) {
      dropdown.classList.toggle("show");
      if (dropdown.classList.contains("show")) {
        document.documentElement.addEventListener("click", closeDropdown);
      }
      else {
        document.documentElement.removeEventListener("click", closeDropdown);
      }
    }
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdown(e) {
  const dropdown = document.querySelector("div[id$='version-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    const currentTarget = e.currentTarget || {};
    const currentTargetParent = currentTarget.closest("div[id$='version-dropdown']");
    const dropdownParent = dropdown.closest("div[id$='version-dropdown']");
    if (currentTargetParent !== dropdownParent) {
      dropdown.classList.remove("show");
    }
    document.documentElement.removeEventListener("click", closeDropdown);
  }
}
