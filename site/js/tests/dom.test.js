import { initPage, updatePage } from "../lib/dom";
import { toDataTable } from "../lib/datatable.js";

/*
This is the shape of the data we want to display in the table.
*/
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

const myRows = toDataTable(archetypeData);

test("initPage inserts table content", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  initPage(myRows);

  const container = document.getElementById("table-container");
  const table = container.querySelector("table");
  
  // Check table exists
  expect(table).not.toBeNull();
  
  // Check headers
  const headers = table.querySelectorAll("thead th");
  expect(headers.length).toBe(5); // Build, Suite, Created At, Updated At, State
  expect(headers[0].textContent).toBe("Build");
  expect(headers[1].textContent).toBe("Suite");
  expect(headers[2].textContent).toBe("Created At");
  expect(headers[3].textContent).toBe("Updated At");
  expect(headers[4].textContent).toBe("State");
  
  // Check data rows (2 builds × 2 suites = 4 rows)
  const rows = table.querySelectorAll("tbody tr");
  expect(rows.length).toBe(4);
});

test("updatePage adds new rows at top of table with hidden class", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  // First create initial table
  initPage(myRows);

  // Create new data with one build
  const newData = {
    "xyz": {
      "fast_suite": {
        "created_at": "2025-02-22T21:48:43.193578Z", 
        "state": "init",
        "updated_at": "2025-02-22T21:48:43.193578Z"
      },
      "slow_suite": {
        "created_at": "2025-02-22T21:48:43.193578Z",
        "state": "init", 
        "updated_at": "2025-02-22T21:48:43.193578Z"
      }
    }
  };

  const newRows = toDataTable(newData);
  updatePage(newRows);

  const table = document.querySelector("table");
  const rows = table.querySelectorAll("tbody tr");

  // Should now have 6 total rows (4 original + 2 new)
  expect(rows.length).toBe(6);

  // New rows should be first and have hidden class
  expect(rows[0].classList.contains("hidden")).toBe(true);
  expect(rows[1].classList.contains("hidden")).toBe(true);
  
  // First row should contain new build data
  const firstRowCells = rows[0].querySelectorAll("td");
  expect(firstRowCells[0].textContent).toBe("xyz");
});
