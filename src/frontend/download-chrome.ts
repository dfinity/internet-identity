#!/usr/bin/env node

import { get } from "https";
import { homedir, platform as osPlatform } from "os";
import { promisify } from "util";
import { mkdirSync, createWriteStream } from "fs";
import { stat, unlink } from "fs/promises";
import stream from "node:stream";
import extract from "extract-zip";
import { unreachable } from "./src/utils/utils";

const pipeline = promisify(stream.pipeline);

const installPath = `${homedir()}/.local-chromium`;

type Config = { platform: "linux" | "darwin"; revision: string };

/* This downloads Chromium from the Chromium build archives
 * For more info on finding a particular version of chromium, see https://www.chromium.org/getting-involved/download-chromium/
 * This also caches chromium downloads. The following happens:
 * - check module cache
 * -    if exists, return
 * - check global cache
 * -    if exists, copy and return
 * - install into global cache
 * - copy and return
 *
 * This is a simplified version of https://github.com/juliangruber/download-chromium, translated
 * to TypeScript in the process.
 */
export async function downloadChrome() {
  const config = getConfig();

  // First, figure out if we have already downloaded chromium

  const moduleExecutablePath = getExecutablePath(config, installPath);
  try {
    await stat(moduleExecutablePath);
    return moduleExecutablePath;
  } catch (_) {
    /* noop */
  }

  // If not, create the cache and download

  try {
    mkdirSync(installPath);
  } catch (_) {
    /* noop */
  }
  const folderPath = getFolderPath(config, installPath);
  const zipPath = `${folderPath}.zip`;

  await pipeline(
    await downloadFromURL(downloadURL(config)),
    createWriteStream(zipPath)
  );

  // Now extract the zip and install it to the cache
  await extract(zipPath, { dir: folderPath });
  await unlink(zipPath);

  return moduleExecutablePath;
}

function getConfig(): Config {
  const platform = osPlatform();

  if (platform !== "linux" && platform !== "darwin") {
    throw new Error(`Platform ${platform} is not supported`);
  }

  // Follow steps here to update: https://www.chromium.org/getting-involved/download-chromium/
  // (or use the cypress helper: https://chromium.cypress.io)
  // This corresponds to version 106
  const revision = platform === "darwin" ? "1036822" : "1036826";

  return { platform, revision };
}

/** The chromium download URL for the given platform and revision */
function downloadURL(config: Config): string {
  if (config.platform === "darwin") {
    return `https://www.googleapis.com/storage/v1/b/chromium-browser-snapshots/o/Mac%2F${config.revision}%2Fchrome-mac.zip?alt=media`;
  } else if (config.platform === "linux") {
    return `https://www.googleapis.com/storage/v1/b/chromium-browser-snapshots/o/Linux_x64%2F${config.revision}%2Fchrome-linux.zip?alt=media`;
  } else return unreachable(config.platform, "unexpected platform in config");
}

/** Name of the archive (depends on platform) */
function archiveName(config: Config): string {
  if (config.platform === "linux") return "chrome-linux";
  else if (config.platform === "darwin") return "chrome-mac";
  else return unreachable(config.platform, "unexpected platform in config");
}

function getFolderPath(config: Config, root: string) {
  return `${root}/chromium-${config.platform}-${config.revision}`;
}

/** Path to exe in archive (depends on platform) */
function getExecutablePath(config: Config, root: string): string {
  const folder = getFolderPath(config, root);
  const archiveFolder = archiveName(config);

  if (config.platform === "darwin") {
    return `${folder}/${archiveFolder}/Chromium.app/Contents/MacOS/Chromium`;
  }
  if (config.platform === "linux") {
    return `${folder}/${archiveFolder}/chrome`;
  }

  return unreachable(config.platform);
}

/** Download a file from a URL */
const downloadFromURL = (url: string): Promise<Buffer> => {
  return new Promise((resolve, reject) => {
    get(url, async (res) => {
      if (res.statusCode !== undefined && [301, 302].includes(res.statusCode)) {
        await downloadFromURL(res.headers.location!).then(resolve, reject);
      }

      const data: any[] = [];

      res.on("data", (chunk) => data.push(chunk));
      res.on("end", () => {
        resolve(Buffer.concat(data));
      });
      res.on("error", reject);
    });
  });
};
