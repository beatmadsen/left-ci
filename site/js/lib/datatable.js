export function toDataTable(data, changedBuilds) {
  const table = [];
  Object.entries(data).forEach(([buildKey, buildValue]) => {
    Object.entries(buildValue).forEach(([suiteName, suiteData]) => {
      const metadata = toMetadata(buildKey, changedBuilds);
      const nextRow = toRow(metadata, buildKey, suiteName, suiteData);
      table.push(nextRow);
    });
  });

  // Sort descending
  table.sort(byUpdatedAtAndSuiteName);
  return table;
}

function toMetadata(buildKey, changedBuilds) {
  return { changed: changedBuilds.includes(buildKey) };
}

function toRow(metadata, buildKey, suiteName, suiteData) {
  return [metadata, suiteData.version, buildKey, suiteName, suiteData.created_at, suiteData.updated_at, suiteData.elapsed_time, suiteData.state];
}

function byUpdatedAtAndSuiteName(a, b) {
  const [,, suiteNameA, , updatedAtA] = a;
  const [,, suiteNameB, , updatedAtB] = b;
  // First compare by updatedAt
  if (updatedAtA !== updatedAtB) {
    return updatedAtA < updatedAtB ? 1 : -1;
  }
  // If updatedAt is equal, compare by suiteName
  return suiteNameA.localeCompare(suiteNameB);
}