import { toDataTable } from "./datatable.js";

export function updatePage(data) {
  const tableContainer = document.getElementById("table-container");
  const tableData = toDataTable(data);  
  const table = createTable(tableData);

  tableContainer.innerHTML = "";
  tableContainer.appendChild(table);
}

function createTable(tableData) {
  const table = document.createElement("table");
  table.appendChild(createTHead());
  table.appendChild(createTBody(tableData));
  return table;
}

function createHeaderRow() {
  const headerCells = ["Build", "Suite", "Created At", "Updated At", "State"];
  
  const headerRow = document.createElement("tr");
  headerCells.forEach(cell => {
    const headerCell = document.createElement("th");
    headerCell.textContent = cell;
    headerRow.appendChild(headerCell);
  });
  return headerRow;
}

function createTHead() {
  const thead = document.createElement("thead");
  thead.appendChild(createHeaderRow());
  return thead;
}

function createTBody(tableData) {
  const tbody = document.createElement("tbody");
  tableData.forEach(row => {
    const rowElement = document.createElement("tr");
    row.forEach(cell => {
      const cellElement = document.createElement("td");
      cellElement.textContent = cell; 
      rowElement.appendChild(cellElement);
    });
    tbody.appendChild(rowElement);
  });
  return tbody;
}