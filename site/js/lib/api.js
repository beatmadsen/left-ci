export async function fetchBuilds(projectId, fetchFn = fetch) {
  try {
    console.log("fetching builds");
    const response = await fetchFn(`/projects/${projectId}/builds`, {
      method: "GET"
    });
    if (!response.ok) { throw new Error("Response not 200 OK"); }
    
    if (!response.headers.get("content-type").includes("application/json")) {
      throw new Error("Response is not json");
    }
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

