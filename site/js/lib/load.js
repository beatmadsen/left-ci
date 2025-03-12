import { fetchBuilds } from "./api.js";
import { BuildHistory } from "./model.js";
import { initPage, refreshTable, revealRows } from "./dom.js";

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
    initPage(history.rows());
    await revealRows();

    // run loadUpdates every 10 seconds
    setInterval(loadUpdates, 10000);
  } catch (e) {
    loadFailed(e.message);
    console.error(e);
    throw e;
  }
}

async function loadUpdates() {
  const p1 = history.update();
  await playUpdateAnimation();
  await p1;
  refreshTable(history.rows());
  await revealRows();
}

function initHistory(projectId) {
  if (projectId === "dummy") {
    history = new BuildHistory(projectId, fetchDummyBuilds);
  } else {
    history = new BuildHistory(projectId);
  }
}


async function fetchDummyBuilds(_projectId, { after = null } = {}) {
  const archetypeData = {
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
  if (after) {
    const newBuild = Math.random().toString(36).substring(2, 5);
    const newUpdatedAt = new Date().toISOString();
    return {
      [newBuild]: {
        ...archetypeData["abc"],
        "slow_suite": {
          ...archetypeData["abc"]["slow_suite"],
          "updated_at": newUpdatedAt
        }
      }
    }
  }
  return archetypeData
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

async function playUpdateAnimation() {
  const smallContainer = document.querySelector("#small-image-container");
  smallContainer.classList.remove('slowly-rotating');
  smallContainer.classList.add('rotating');
  await wait(600);
  smallContainer.classList.remove('rotating');
  smallContainer.classList.add('slowly-rotating');
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
