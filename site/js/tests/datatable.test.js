import { toDataTable } from "../lib/datatable";

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

test("should flatten archetype data to data table", () => {
  const dataTable = toDataTable(archetypeData);
  expect(dataTable).toEqual([
    ["abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],
    ["abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],
    ["aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"],
    ["aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"]
  ]);
});
