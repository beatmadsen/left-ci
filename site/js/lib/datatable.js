export function toDataTable(data) {
  const table = [];
  Object.entries(data).forEach(([buildKey, buildValue]) => {
    Object.entries(buildValue).forEach(([suiteName, suiteData]) => {
      const row = [buildKey, suiteName, suiteData.created_at, suiteData.updated_at, suiteData.state];
      table.push(row);
    });
  });
  return table;
}

