import { fetchBuilds } from "./api.js";
import { BuildHistory } from "./model.js";
import { updatePage, revealRows } from "./dom.js";

let history;

export async function load() {
  try {
    console.log("load");
    const projectId = getProjectId();
    initHistory(projectId);
    const p1 = history.update();
    await playLoadAnimation();
    await p1;
    await playFadeAwayAnimation();
    moveImageToSmallContainer();
    updatePage(history.rows());
    await revealRows();
  } catch (e) {
    loadFailed(e.message);
    console.error(e);
    throw e;
  }
}

function initHistory(projectId) {
  if (projectId === "dummy") {
    history = new BuildHistory(projectId, fetchDummyBuilds);
  } else {
    history = new BuildHistory(projectId);
  }
}

export async function loadChanges() {
  const projectId = getProjectId();
  const after = getAfter();
  /* NB: 
  this could be builds already in the table that have entered a new state
  or it could be new builds that should be added to the table
  */
  const moreBuilds = await fetchBuilds(projectId, { after });
  /* NB:
  we should keep the model around and render it as html tags
  every time it's updated.
  TODO: merge the two hashmaps and then re-animate the combined model
  */
  updateTable(moreBuilds);
}

async function fetchDummyBuilds(_projectId, { after = null } = {}) {
  console.log("fetchDummyBuilds", after);
  return {
    "abc": {
      "fast_suite": {
        "created_at": "2025-02-22T10:44:40.160377Z",
        "state": "init",
        "updated_at": "2025-02-22T10:44:40.160377Z"
      },
      "slow_suite": {
        "created_at": "2025-02-22T10:44:40.160377Z",
        "state": "init",
        "updated_at": "2025-02-22T10:44:40.160377Z"
      }
    },
    "aidfudb": {
      "fast_suite": {
        "created_at": "2025-02-22T20:48:43.193578Z",
        "state": "init",
        "updated_at": "2025-02-22T20:48:43.193578Z"
      },
      "slow_suite": {
        "created_at": "2025-02-22T20:48:43.193578Z",
        "state": "init",
        "updated_at": "2025-02-22T20:48:43.193578Z"
      }
    }
  }
}

function moveImageToSmallContainer() {
  const img = document.querySelector("#table-container img");
  const smallContainer = document.querySelector("#small-image-container");
  smallContainer.innerHTML = `<img src="${img.src}" alt="${img.alt}" class="small-round-image fade-in"/>`;
}

async function playFadeAwayAnimation() {
  const img = document.querySelector("#table-container img");
  img.classList.add("fade-away");
  await wait(300);
}

async function playLoadAnimation() {
  return wait(1600);
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
