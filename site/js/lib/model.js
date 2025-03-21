import { fetchBuilds } from "./api.js";
import { toDataTable } from "./datatable.js";

export class BuildHistory {
  constructor(projectId, fetchFn = fetchBuilds) {
    this.projectId = projectId;
    this.builds = {};
    this.fetchFn = fetchFn;
    this.changedBuilds = [];
    this.tenLatestBuilds = [];
  }

  async update() {
    if (Object.keys(this.builds).length === 0) {
      const builds = await this.fetchFn(this.projectId);
      this.changedBuilds = Object.keys(builds);
      this.builds = enrichBuilds(builds);
    } else {
      const latestUpdate = this.#getLatestUpdate();
      const builds = await this.fetchFn(this.projectId, { after: latestUpdate });
      if (!builds || builds.length === 0) {
        this.changedBuilds = [];
      } else {
        this.changedBuilds = Object.keys(builds);
        console.log(`Fetched ${this.changedBuilds.length} builds after ${latestUpdate}`);
        this.builds = { ...this.builds, ...enrichBuilds(builds) };
      }
    }
    this.#updateTenLatestBuilds();
    this.#trimBuilds();
  }

  updateElapsedTime() {    
    this.builds = populateElapsedTime(this.builds);
  }

  changedRows() {
    // get the builds map with just the keys in this.changedBuilds
    const subMap = {};
    for (const buildKey of this.changedBuilds) {
      subMap[buildKey] = this.builds[buildKey];
    }
    return toDataTable(subMap, this.changedBuilds);
  }

  rows() {
    return toDataTable(this.builds, this.changedBuilds);
  }

  #trimBuilds() {
    const subMap = {};
    for (const buildKey of this.tenLatestBuilds) {
      subMap[buildKey] = this.builds[buildKey];
    }
    this.builds = subMap;
  }

  #updateTenLatestBuilds() {
    for (const buildKey of this.changedBuilds) {
      if (!this.tenLatestBuilds.includes(buildKey)) {
        this.tenLatestBuilds.push(buildKey);
        if (this.tenLatestBuilds.length > 10) {
          this.tenLatestBuilds.shift();
        }
      }
    }
  }

  #getLatestUpdate() {
    let latestUpdate = 0;
    for (const build of Object.values(this.builds)) {
      for (const suite of Object.values(build)) {        
        if (suite.updated_at > latestUpdate) {
          latestUpdate = suite.updated_at;
        }
      }
    }
    return latestUpdate;
  }  
}

function enrichBuilds(builds) {
  return reviveDates(builds);
}

export function populateElapsedTime(builds) {
  const now = new Date();
  return Object.fromEntries(Object.entries(builds).map(([buildKey, suites]) => [buildKey, populateElapsedTimeInSuites(suites, now)]));
}

function populateElapsedTimeInSuites(suites, now) {
  return Object.fromEntries(Object.entries(suites).map(([suiteName, suite]) => [suiteName, populateElapsedTimeInSuite(suite, now)]));
}

function populateElapsedTimeInSuite(suite, now) {
  const cloned = { ...suite };
  const inFinalState = ["failed", "passed"].includes(cloned.state);
  const elapsedMs = inFinalState ? cloned.updated_at - cloned.created_at : now - cloned.created_at;
  cloned.elapsed_time = formatDuration(elapsedMs);
  return cloned;
}

function formatDuration(ms) {
  const seconds = Math.floor(ms / 1000);
  const hours = Math.floor(seconds / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const remainingSeconds = seconds % 60;
  
  const parts = [];
  if (hours > 0) parts.push(`${hours}h`);
  if (minutes > 0) parts.push(`${minutes}m`);
  if (remainingSeconds > 0) parts.push(`${remainingSeconds}s`);
  
  return parts.join(' ') || '0s';
}

export function reviveDates(builds) {
  return Object.fromEntries(Object.entries(builds).map(([buildKey, suites]) => [buildKey, reviveDatesInSuites(suites)]));
}

function reviveDatesInSuites(suites) {
  return Object.fromEntries(Object.entries(suites).map(([suiteName, suite]) => [suiteName, reviveDatesInSuite(suite)]));
}

function reviveDatesInSuite(suite) {
  const cloned = { ...suite };
  cloned.created_at = new Date(cloned.created_at);
  cloned.updated_at = new Date(cloned.updated_at);
  return cloned;
}
