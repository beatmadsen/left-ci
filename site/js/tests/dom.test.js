import { initPage, refreshTable } from "../lib/dom";
import { toDataTable } from "../lib/datatable.js";

/*
This is the shape of the data we want to display in the table.
*/
const archetypeData = {
  "abc": {
    "fast_suite": {
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z",
      "version": "04a66b1n"
    },
    "slow_suite": {
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z",
      "version": "04a66b1n"
    }
  },
  "aidfudb": {
    "fast_suite": {
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z",
      "version": "01b66b1n"
    },
    "slow_suite": {
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z",
      "version": "01b66b1n"
    }
  }
}

const myRows = toDataTable(archetypeData, []);

test("initPage inserts table content", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  initPage(myRows);

  const container = document.getElementById("table-container");
  const table = container.querySelector("table");
  
  // Check table exists
  expect(table).not.toBeNull();
  
  // Check headers
  const headers = table.querySelectorAll("thead th");
  expect(headers.length).toBe(7);
  expect(headers[0].textContent).toBe("Version");
  expect(headers[1].textContent).toBe("Build");
  expect(headers[2].textContent).toBe("Suite");
  expect(headers[3].textContent).toBe("Created At");
  expect(headers[4].textContent).toBe("Updated At");
  expect(headers[5].textContent).toBe("Elapsed Time");
  expect(headers[6].textContent).toBe("State");
  
  // Check data rows (2 builds × 2 suites = 4 rows)
  const rows = table.querySelectorAll("tbody tr");
  expect(rows.length).toBe(4);
});

test("refreshTable replaces all the rows", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  initPage(myRows);
  
  const updatedRows = toDataTable(archetypeData, ["aidfudb"]);
  refreshTable(updatedRows);

  const container = document.getElementById("table-container");
  const table = container.querySelector("table");
  const rows = table.querySelectorAll("tbody tr");
  expect(rows.length).toBe(4);
  
});

test("refreshTable reveals rows that have changed", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  initPage(myRows);

  const updatedRows = toDataTable(archetypeData, ["aidfudb"]);
  refreshTable(updatedRows);

  const container = document.getElementById("table-container");
  const table = container.querySelector("table");
  const rows = table.querySelectorAll("tbody tr.hidden");
  expect(rows.length).toBe(2);
});