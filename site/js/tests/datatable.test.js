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
    [{ changed: false }, "aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"],
    [{ changed: false }, "aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"],
    [{ changed: false }, "abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],  
    [{ changed: false }, "abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],
  ]);
});

test("should add a column to the data table to indicate if the build has changed", () => {
  const dataTable = toDataTable(archetypeData, ["aidfudb"]);
  expect(dataTable).toEqual([
    [{ changed: true }, "aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"],
    [{ changed: true }, "aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "init"],
    [{ changed: false }, "abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],  
    [{ changed: false }, "abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "init"],
  ]);
});

