import { toDataTable } from "./datatable";
/* a reminder that this is the shape of the data we want to display in the table */
const archetypeData = {
  "abc": { // build global_id
    "fast_suite": { // suite name
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z"
    },
    "slow_suite": { // suite name
      "created_at": "2025-02-22T10:44:40.160377Z",
      "state": "init",
      "updated_at": "2025-02-22T10:44:40.160377Z"
    }
  },
  "aidfudb": { // build global_id
    "fast_suite": { // suite name
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z"
    },
    "slow_suite": { // suite name
      "created_at": "2025-02-22T20:48:43.193578Z",
      "state": "init",
      "updated_at": "2025-02-22T20:48:43.193578Z"
    }
  }
}


export function updatePage(data) {
  const tableContainer = document.getElementById("table-container");
  const headerCells = ["Build", "Suite", "Created At", "Updated At", "State"];
  
  const headerRow = document.createElement("tr");
  headerCells.forEach(cell => {
    const headerCell = document.createElement("th");
    headerCell.textContent = cell;
    headerRow.appendChild(headerCell);
  });

  const table = document.createElement("table");
  table.appendChild(headerRow);

  const tbody = document.createElement("tbody");
  const tableData = toDataTable(data);
  tableData.forEach(row => {
    const rowElement = document.createElement("tr");
    row.forEach(cell => {
      const cellElement = document.createElement("td");
      cellElement.textContent = cell; 
      rowElement.appendChild(cellElement);
    });
    tbody.appendChild(rowElement);
  });
  table.appendChild(tbody);

  tableContainer.innerHTML = "";
  tableContainer.appendChild(table);
}
