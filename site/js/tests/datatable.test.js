import { toDataTable } from "../lib/datatable";

const archetypeData = {
  "abc": {
    "fast_suite": {
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z",
      "version": "04a66b1n",
      "elapsed_time": "12 seconds"
    },
    "slow_suite": {
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z",
      "version": "04a66b1n",
      "elapsed_time": "30 minutes"
    }
  },
  "aidfudb": {
    "fast_suite": {
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z",
      "version": "01b66b1n",
      "elapsed_time": "2 minutes"
    },
    "slow_suite": {
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z",
      "version": "01b66b1n",
      "elapsed_time": "1 hour"
    }
  }
}

test("should flatten archetype data to data table ordered by updated_at and suite name", () => {
  const dataTable = toDataTable(archetypeData, []);
  expect(dataTable).toEqual([
    [{ changed: false }, "01b66b1n", "aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "2 minutes", "init"],
    [{ changed: false }, "01b66b1n", "aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "1 hour", "init"],
    [{ changed: false }, "04a66b1n", "abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "12 seconds", "init"],  
    [{ changed: false }, "04a66b1n", "abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "30 minutes", "init"],
  ]);
});

test("should add a column to the data table to indicate if the build has changed", () => {
  const dataTable = toDataTable(archetypeData, ["aidfudb"]);
  expect(dataTable).toEqual([
    [{ changed: true }, "01b66b1n", "aidfudb", "fast_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "2 minutes", "init"],
    [{ changed: true }, "01b66b1n", "aidfudb", "slow_suite", "2025-02-22T20:48:43.193578Z", "2025-02-22T20:48:43.193578Z", "1 hour", "init"],
    [{ changed: false }, "04a66b1n", "abc", "fast_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "12 seconds", "init"],  
    [{ changed: false }, "04a66b1n", "abc", "slow_suite", "2025-02-22T10:44:40.160377Z", "2025-02-22T10:44:40.160377Z", "30 minutes", "init"],
  ]);
});

