export function initPage(rows) {
  const tableContainer = document.getElementById("table-container");
  const table = createTable(rows);

  tableContainer.innerHTML = "";
  tableContainer.appendChild(table);
}

export function refreshTable(rows) {
  // find existing table
  const table = document.querySelector("table");
  const tbody = table.querySelector("tbody");

  // remove all existing rows
  tbody.innerHTML = "";

  // create new rows
  rows.forEach(row => {
    const rowElement = createRow(row);
    tbody.appendChild(rowElement);
  });

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
    const rowElement = createRow(row);
    tbody.appendChild(rowElement);
  });
  return tbody;
}

function createRow(tableDataRow) {
  const rowElement = document.createElement("tr");
  const metadata = tableDataRow[0];
  const isUpdated = metadata.changed;
  if (isUpdated) {    
    rowElement.classList.add("hidden");
  }
  tableDataRow.slice(1, 6).forEach(cell => {
    const cellElement = document.createElement("td");
    cellElement.textContent = cell;
    rowElement.appendChild(cellElement);
  });
  return rowElement;
}

export async function revealRows() {
  const rows = document.querySelectorAll("tbody tr.hidden");
  for (const row of rows) {
    await new Promise(resolve => setTimeout(resolve, 150));
    row.classList.remove("hidden");
    row.classList.add("fade-slide-in");
  }
}