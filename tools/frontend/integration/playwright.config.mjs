import { defineConfig, devices } from "@playwright/test";
import { fileURLToPath } from "node:url";

export default defineConfig({
  testDir: ".",
  testMatch: "*.spec.mjs",
  workers: 1,
  fullyParallel: false,
  forbidOnly: !!process.env.CI,
  retries: 0,
  timeout: 30000,
  outputDir: "../../../target/browser-integration/results",
  reporter: [
    ["list"],
    [
      "junit",
      {
        outputFile: fileURLToPath(
          new URL(
            "../../../target/test-results/browser-integration.xml",
            import.meta.url,
          ),
        ),
      },
    ],
  ],
  use: { trace: "retain-on-failure", screenshot: "only-on-failure" },
  projects: [
    {
      name: "desktop-chromium",
      use: {
        ...devices["Desktop Chrome"],
        viewport: { width: 1440, height: 900 },
        channel: process.env.LLAR_BROWSER_CHANNEL ?? "chromium",
      },
    },
    {
      name: "mobile-chromium",
      use: {
        ...devices["Pixel 7"],
        channel: process.env.LLAR_BROWSER_CHANNEL ?? "chromium",
      },
    },
    {
      name: "mobile-webkit",
      use: { ...devices["iPhone 13"], defaultBrowserType: "webkit" },
    },
  ],
});
