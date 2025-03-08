import { BuildHistory, reviveDates } from "../lib/model";
import { jest } from "@jest/globals";

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

test("given an empty history, when updating, then the history is populated", async () => {
  const history = new BuildHistory("my-project", () => archetypeData);
  await history.update();
  expect(history.rows()).toHaveLength(4);
});

test("given a history with some builds, when updating, then the history is updated", async () => {
  const firstUpdate = archetypeData;
  const secondUpdate = {
    ...archetypeData,
    "abc": {
      ...archetypeData["abc"],
      "slow_suite": { ...archetypeData["abc"]["slow_suite"], "state": "success", "updated_at": "2025-03-06T22:48:43.193578Z" }
    },
    "def": { ...archetypeData["abc"] }
  }

  const fetchFn = jest.fn();
  fetchFn.mockResolvedValueOnce(firstUpdate);
  fetchFn.mockResolvedValueOnce(secondUpdate);
  const history = new BuildHistory("my-project", fetchFn);
  await history.update();
  await history.update();
  expect(history.rows()).toHaveLength(6);
})


test("given a history with some builds, when updating, then we fetch the builds after the latest build", async () => {
  const firstUpdate = { ...archetypeData, "abc": { ...archetypeData["abc"], "slow_suite": { ...archetypeData["abc"]["slow_suite"], "updated_at": "2025-03-22T20:48:43.193578Z" } } };
  const secondUpdate = {}

  const fetchFn = jest.fn();
  fetchFn.mockResolvedValueOnce(firstUpdate);
  fetchFn.mockResolvedValueOnce(secondUpdate);
  const history = new BuildHistory("my-project", fetchFn);
  await history.update();
  await history.update();
  expect(fetchFn).toHaveBeenCalledWith("my-project", { after: new Date("2025-03-22T20:48:43.193578Z") });
})

test("given fetched builds, when parsing, then dates are revived", () => {
  const builds = {
    "abc": {
      "fast_suite": { "created_at": "2025-02-22T10:44:40.160377Z", "updated_at": "2025-02-22T10:44:40.160377Z" }
    }
  }
  const parsed = reviveDates(builds);
  expect(parsed).toEqual({ "abc": { "fast_suite": { "created_at": new Date("2025-02-22T10:44:40.160377Z"), "updated_at": new Date("2025-02-22T10:44:40.160377Z") } } });
})

test("given a history with some builds, when updating, then we fetch the builds after the latest build", async () => {
  const firstUpdate = archetypeData;
  const secondUpdate = { "abc": { ...archetypeData["abc"], "slow_suite": { ...archetypeData["abc"]["slow_suite"], "updated_at": "2025-03-23T20:48:43.193578Z" } } };
  const fetchFn = jest.fn();
  fetchFn.mockResolvedValueOnce(firstUpdate);
  fetchFn.mockResolvedValueOnce(secondUpdate);
  const history = new BuildHistory("my-project", fetchFn);
  await history.update();
  await history.update();
  expect(history.changedRows()).toHaveLength(2);
})