import { jest } from '@jest/globals';
import { fetchBuilds, ApiError } from "../lib/api";


test("should call the correct endpoint", async () => {
  const fetchFn = jest.fn();
  fetchFn.mockResolvedValue({
    ok: true,
    headers: {
      get: () => "application/json"
    },
    json: () => Promise.resolve({})
  });

  await fetchBuilds("test", fetchFn);  
  expect(fetchFn).toHaveBeenCalledWith(
    "/projects/test/builds",
    expect.objectContaining({
      method: "GET"
    })
  );
});

test("should throw an ApiError if the response is not ok", async () => {
  const fetchFn = jest.fn();
  fetchFn.mockResolvedValue({
    ok: false,
    statusText: "Not found"
  });

  await expect(fetchBuilds("test", fetchFn)).rejects.toThrow(ApiError);
});

test("should throw an ApiError if the response is not json", async () => {
  const fetchFn = jest.fn();
  fetchFn.mockResolvedValue({
    ok: true,
    headers: { 
      get: () => "text/html"
    },
    json: () => Promise.resolve({})
  });

  await expect(fetchBuilds("test", fetchFn)).rejects.toThrow(ApiError);
});

test("should return the builds", async () => {
  const fetchFn = jest.fn();
  fetchFn.mockResolvedValue({
    ok: true,
    headers: {
      get: () => "application/json"
    },
    json: () => Promise.resolve(archetypeData)
  });

  const builds = await fetchBuilds("test", fetchFn);
  expect(builds).toEqual(archetypeData);
});

test("should catch errors from fetch", async () => {
  const fetchFn = jest.fn();
  fetchFn.mockRejectedValue(new Error("Network error"));

  await expect(fetchBuilds("test", fetchFn)).rejects.toThrow(ApiError);
});




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

