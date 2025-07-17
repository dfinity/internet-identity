import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { formatLastUsage } from "./time";

describe("formatLastUsage", () => {
  const NOW = new Date();

  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
  });

  afterEach(() => {
    vi.useRealTimers();
  });

  test("formats time within the last hour", () => {
    const timestamp = new Date(NOW.getTime() - 30 * 60 * 1000); // 30 minutes ago
    expect(formatLastUsage(timestamp)).toBe(
      `today at ${timestamp.toLocaleTimeString("en-US", {
        hour: "numeric",
        minute: "numeric",
      })}`,
    );
  });

  test("formats time from earlier today", () => {
    const timestamp = new Date(NOW.getTime() - 7 * 60 * 60 * 1000); // 7 hours ago
    expect(formatLastUsage(timestamp)).toBe(
      `today at ${timestamp.toLocaleTimeString("en-US", {
        hour: "numeric",
        minute: "numeric",
      })}`,
    );
  });

  test("formats time from yesterday", () => {
    const timestamp = new Date(NOW.getTime() - 24 * 60 * 60 * 1000); // 24 hours ago
    expect(formatLastUsage(timestamp)).toBe("yesterday");
  });

  test("formats time from several days ago", () => {
    const timestamp = new Date(NOW.getTime() - 5 * 24 * 60 * 60 * 1000); // 5 days ago
    expect(formatLastUsage(timestamp)).toBe("5 days ago");
  });

  test("formats time from last month", () => {
    const timestamp = new Date(NOW.getTime() - 30 * 24 * 60 * 60 * 1000); // ~1 month ago
    expect(formatLastUsage(timestamp)).toBe("last month");
  });

  test("formats time from 5 months ago", () => {
    const timestamp = new Date(NOW.getTime() - 5 * 30 * 24 * 60 * 60 * 1000); // ~1 month ago
    expect(formatLastUsage(timestamp)).toBe("5 months ago");
  });
});
