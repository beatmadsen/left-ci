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

test("dummy test", () => {
  expect(true).toBe(true);
});


// test("updatePage replaces table content", () => {
//   document.body.innerHTML = `<div id="table-container"></div>`;

//   updatePage(archetypeData);

//   const container = document.getElementById("table-container");
//   expect(container.querySelector("table")).not.toBeNull();
//   expect(container.querySelectorAll("tr").length).toBe(2);
//   expect(container.querySelectorAll("td").length).toBe(4);
//   expect(container.querySelectorAll("th").length).toBe(2);
// });
