export function toDataTable(data) {
  const table = [];
  Object.entries(data).forEach(([buildKey, buildValue]) => {
    Object.entries(buildValue).forEach(([suiteName, suiteData]) => {
      const row = [buildKey, suiteName, suiteData.created_at, suiteData.updated_at, suiteData.state];
      table.push(row);
    });
  });

  // Sort descending
  table.sort((a, b) => {
    const [, suiteNameA, , updatedAtA] = a;
    const [, suiteNameB, , updatedAtB] = b;
    // First compare by updatedAt
    if (updatedAtA !== updatedAtB) {
      return updatedAtA < updatedAtB ? 1 : -1;
    }
    // If updatedAt is equal, compare by suiteName
    return suiteNameA.localeCompare(suiteNameB);
  });
  return table;
}