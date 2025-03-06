import { BuildHistory } from "../lib/model";
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