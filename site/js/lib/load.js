import { fetchBuilds } from "./api.js";
import { updatePage, revealRows } from "./dom.js";

export async function load() {
  try {
    console.log("calling main");
    const projectId = getProjectId();
    console.log("projectId", projectId);
    const isDummy = projectId === "dummy"; 
    console.log("isDummy", isDummy);
    const p1 = isDummy ? fetchDummyBuilds() : fetchBuilds(projectId);
    await playLoadAnimation();
    const builds = await p1;
    await playFadeAwayAnimation();
    moveImageToSmallContainer();
    updatePage(builds);
    await revealRows();
  } catch (e) {
    loadFailed(e.message);
    throw e;
  }
}

async function fetchDummyBuilds() {
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
  return wait(5600);
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
