export async function fetchBuilds(
  projectId,
  { after = null, fetchFn = fetch } = {}
) {
  try {
    const afterParam = after ? `?after=${dateForQueryParam(after)}` : "";
    console.log("fetching builds, after query param: ", afterParam);
    const response = await fetchFn(`/projects/${projectId}/builds${afterParam}`, {
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

function dateForQueryParam(date) {
  return encodeURIComponent(date.toISOString());
}
