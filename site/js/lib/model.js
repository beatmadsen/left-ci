import { fetchBuilds } from "./api";
import { toDataTable } from "./datatable";

export class BuildHistory {
  constructor(projectId, fetchFn = fetchBuilds) {
    this.projectId = projectId;
    this.builds = {};
    this.fetchFn = fetchFn;
  }

  async update() {
    if (Object.keys(this.builds).length === 0) {
      const builds = await this.fetchFn(this.projectId);
      this.builds = this.parse(builds);
    } else {
      const latestUpdate = this.#getLatestUpdate();
      const builds = await this.fetchFn(this.projectId, { after: latestUpdate });
      this.builds = { ...this.builds, ...this.#parse(builds) };
    }
  }

  rows() {
    return toDataTable(this.builds);
  }

  #parse(builds) {
    // map date strings to Date objects
    throw new Error("Not implemented");
    
  }

  #getLatestUpdate() {
    throw new Error("Not implemented");
  }
}
