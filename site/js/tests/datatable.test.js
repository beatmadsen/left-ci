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

test("should flatten archetype data to data table ordered by updated_at and suite name", () => {
  const dataTable = toDataTable(archetypeData, []);
  expect(dataTable).toEqual([
    ["aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init", false],
    ["aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init", false],
    ["abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init", false],  
    ["abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init", false],
  ]);
});

test("should add a column to the data table to indicate if the build has changed", () => {
  const dataTable = toDataTable(archetypeData, ["aidfudb"]);
  expect(dataTable).toEqual([
    ["aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init", true],
    ["aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init", true],
    ["abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init", false],  
    ["abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init", false],
  ]);
});

