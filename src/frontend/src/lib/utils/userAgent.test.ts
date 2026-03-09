import { checkRequiredBrowserVersion, supportsWebauthRoR } from "./userAgent";

describe("User Agent Tests", () => {
  const testVectors = [
    // Chrome Test
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) Chrome/110.0.0.0 Safari/537.36",
      versions: { chrome: 100, edge: 100, safari: 17 },
      expected: true,
      expectedSupportRoR: false,
    },

    // Edge Test
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 Edg/127.0.0.0",
      versions: { edge: 128 },
      expected: false,
      expectedSupportRoR: false,
    },
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/127.0.0.0 Safari/537.36 Edg/127.0.0.0",
      versions: { edge: 126 },
      expected: true,
      expectedSupportRoR: false,
    },

    // Safari Test
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 13_0) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15",
      versions: { safari: 18 },
      expected: false,
      expectedSupportRoR: false,
    },
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 13_0) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15",
      versions: { safari: 16 },
      expected: true,
      expectedSupportRoR: false,
    },

    // Firefox Test
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0",
      versions: { firefox: 133 },
      expected: true,
      expectedSupportRoR: false,
    },
    {
      userAgent:
        "Mozilla/5.0 (Android 13; Mobile; rv:132.0) Gecko/132.0 Firefox/132.0",
      versions: { firefox: 133 },
      expected: false,
      expectedSupportRoR: false,
    },

    // Internet Explorer Tests (Not Supported)
    {
      userAgent:
        "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; Trident/6.0)",
      versions: { chrome: 130, edge: 130, safari: 17 },
      expected: false,
      expectedSupportRoR: false,
    },
    {
      userAgent:
        "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.0; Trident/4.0)",
      versions: { chrome: 130, edge: 130, safari: 17 },
      expected: false,
      expectedSupportRoR: false,
    },

    // Samsung Browser Tests
    {
      userAgent:
        "Mozilla/5.0 (Linux; Android 10; SM-G975F) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Mobile Safari/537.36 SamsungBrowser/19.0",
      versions: { chrome: 112, edge: 111 },
      expected: true,
      expectedSupportRoR: false,
    },
    {
      userAgent:
        "Mozilla/5.0 (Linux; Android 9; SM-G960F) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/80.0.0.0 Mobile Safari/537.36 SamsungBrowser/10.0",
      versions: { chrome: 112, edge: 111, safari: 18 },
      expected: false,
      expectedSupportRoR: false,
    },

    // No Matching Browser (Random String)
    {
      userAgent: "UnknownBrowser/123.456",
      versions: { chrome: 112, edge: 111, safari: 18 },
      expected: false,
      expectedSupportRoR: false,
    },
    {
      userAgent: "",
      versions: { chrome: 112, edge: 111, safari: 18 },
      expected: false,
      expectedSupportRoR: false,
    }, // Empty user agent

    // Edge Tests
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/110.0.0.0 Safari/537.36 Edg/111.0.0.0",
      versions: { edge: 110 },
      expected: true,
      expectedSupportRoR: false,
    },

    // Support Webauth Related Origin Requests
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15",
      versions: { safari: 18 },
      expected: true,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36",
      versions: { chrome: 128 },
      expected: true,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
      versions: { chrome: 130 },
      expected: true,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
      versions: { chrome: 131 },
      expected: false,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36 Edg/128.0.0.0",
      versions: { edge: 128 },
      expected: true,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
      versions: { chrome: 130, edge: 100, safari: 17 },
      expected: true,
      expectedSupportRoR: true,
    },
    {
      userAgent:
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
      versions: { chrome: 128, edge: 100, safari: 17 },
      expected: true,
      expectedSupportRoR: true,
    },
  ];

  testVectors.forEach(
    ({ userAgent, versions, expected, expectedSupportRoR }, index) => {
      it(`Test user agent #${index}: Expected ${expected}`, () => {
        const result = checkRequiredBrowserVersion(userAgent, versions);
        expect(result).toBe(expected);
        const supportsRoR = supportsWebauthRoR(userAgent);
        expect(supportsRoR).toBe(expectedSupportRoR);
      });
    },
  );
});
