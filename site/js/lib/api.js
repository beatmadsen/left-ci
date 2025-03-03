export async function fetchBuilds(projectId, fetchFn = fetch) {
  try {
    const response = await fetchFn(`/projects/${projectId}/builds`, {
      method: "GET"
    });
    if (!response.ok) { throw new ApiError(); }
    return response.json();
  } catch (error) {
    throw new ApiError({ cause: error });
  }
}

export class ApiError extends Error {
  constructor(options = {}) {
    super("Failed to load builds", options);
  }
}

