import { updatePage } from "../lib/dom";

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

test("updatePage replaces table content", () => {
  document.body.innerHTML = `<div id="table-container"></div>`;

  updatePage(archetypeData);

  const container = document.getElementById("table-container");
  const table = container.querySelector("table");
  
  // Check table exists
  expect(table).not.toBeNull();
  
  // Check headers
  const headers = table.querySelectorAll("th");
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
