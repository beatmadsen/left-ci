import { fetchBuilds } from "./api.js";
import { updatePage } from "./dom.js";

export async function load() {
  try {
    console.log("calling main");
    const projectId = getProjectId();
    const p1 = fetchBuilds(projectId);
    await wait(5600); // play animation while fetching
    const builds = await p1;
    updatePage(builds);
  } catch (e) {
    loadFailed(e.message);
    throw e;
  }
}

async function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function getProjectId() {
  const urlParams = new URLSearchParams(window.location.search);
  const projectId = urlParams.get("projectId");
  if (!projectId) {
    throw new MissingProjectIdError();
  }
  return projectId;
}

class MissingProjectIdError extends Error {
  constructor() { super("No project id provided"); }
}

function loadFailed(message) {
  // update table-container img to show a red glow
  const img = document.querySelector("#table-container img");
  console.log("img", img);
  img.classList.remove("loading-image");
  img.classList.add("red-glow");
  // add a text field on inside the table-container saying "Failed to load"
  const tableContainer = document.querySelector("#table-container");
  // append text as child of tableContainer
  const text = document.createElement("div");
  text.textContent = message;
  text.classList.add("failed-text");
  tableContainer.appendChild(text);
}
