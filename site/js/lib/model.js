import { fetchBuilds } from "./api.js";
import { toDataTable } from "./datatable.js";

export class BuildHistory {
  constructor(projectId, fetchFn = fetchBuilds) {
    this.projectId = projectId;
    this.builds = {};
    this.fetchFn = fetchFn;
  }

  async update() {
    console.log("update", this.projectId);
    if (Object.keys(this.builds).length === 0) {
      const builds = await this.fetchFn(this.projectId);
      this.builds = reviveDates(builds);
    } else {
      const latestUpdate = this.#getLatestUpdate();
      const builds = await this.fetchFn(this.projectId, { after: latestUpdate });
      this.builds = { ...this.builds, ...reviveDates(builds) };
    }
  }

  rows() {
    return toDataTable(this.builds);
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