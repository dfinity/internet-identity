#!/usr/bin/env node
import * as os from "os";
import { promisify } from "util";
import * as fs from "fs";
import got from "got";
import stream from "node:stream";
import extract from "extract-zip";
import { unreachable } from "./src/utils/utils";

const pipeline = promisify(stream.pipeline);

const stat = promisify(fs.stat);
const unlink = promisify(fs.unlink);

const cacheRoot = `${os.homedir()}/.chromium-cache`;
const installPath = `${os.homedir()}/.local-chromium`;

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

  const globalExecutablePath = getExecutablePath(config, cacheRoot);
  let exists = false;
  try {
    await stat(globalExecutablePath);
    exists = true;
  } catch (_) {
    /* noop */
  }

  if (exists) {
    await copyCacheToModule(config, moduleExecutablePath, installPath);
    return moduleExecutablePath;
  }

  // If not, create the cache and download

  try {
    fs.mkdirSync(cacheRoot);
  } catch (_) {
    /* noop */
  }
  const folderPath = getFolderPath(config, cacheRoot);
  const zipPath = `${folderPath}.zip`;

  await pipeline(
    await got.stream(downloadURL(config)),
    fs.createWriteStream(zipPath)
  );

  // Now extract the zip and install it to the cache
  await extract(zipPath, { dir: folderPath });
  await unlink(zipPath);
  await copyCacheToModule(config, moduleExecutablePath, installPath);

  return moduleExecutablePath;
}

async function copyCacheToModule(
  config: Config,
  moduleExecutablePath: string,
  installPath: string
) {
  if (!fs.existsSync(getFolderPath(config, installPath))) {
    fs.mkdirSync(getFolderPath(config, installPath), {
      recursive: true,
    });
  }

  fs.cpSync(
    getFolderPath(config, cacheRoot),
    getFolderPath(config, installPath),
    { recursive: true }
  );
}

function getConfig(): Config {
  const platform = os.platform();

  if (platform !== "linux" && platform !== "darwin") {
    throw new Error(`Platform ${platform} is not supported`);
  }

  // Follow steps here to update: https://www.chromium.org/getting-involved/download-chromium/
  const revision = platform === "darwin" ? "1036822" : "1036826";

  return { platform, revision };
}

/** The chromium download URL for the given platform and revision */
function downloadURL(config: Config) {
  if (config.platform === "darwin") {
    return `https://www.googleapis.com/storage/v1/b/chromium-browser-snapshots/o/Mac%2F${config.revision}%2Fchrome-mac.zip?alt=media`;
  } else if (config.platform === "linux") {
    return `https://www.googleapis.com/storage/v1/b/chromium-browser-snapshots/o/Linux_x64%2F${config.revision}%2Fchrome-linux.zip?alt=media`;
  }
}

/** Name of the archive (depends on platform) */
function archiveName(config: Config) {
  if (config.platform === "linux") return "chrome-linux";
  if (config.platform === "darwin") return "chrome-mac";
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
