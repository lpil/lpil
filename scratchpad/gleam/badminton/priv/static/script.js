const hamburger = document.querySelector("[data-hamburger]");

hamburger.addEventListener("click", () => {
  document.body.dataset.sidebarOpen = true;
});

document.addEventListener("click", (event) => {
  if (event.composedPath().includes(hamburger)) {
    return;
  }
  delete document.body.dataset.sidebarOpen;
});

document.addEventListener("keydown", (event) => {
  if (event.key === "Escape") {
    delete document.body.dataset.sidebarOpen;
  }
});
