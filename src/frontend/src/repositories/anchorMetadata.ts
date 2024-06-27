import { MetadataMapV2 } from "$generated/internet_identity_types";
import type { AuthenticatedConnection } from "$src/utils/iiConnection";

export type AnchorMetadata = {
  recoveryPageShownTimestampMillis?: number;
};

const RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS = "recoveryPageShownTimestampMillis";
const METADATA_FETCH_TIMEOUT = 1_000;
const METADATA_FETCH_RETRIES = 10;

const convertMetadata = (rawMetadata: MetadataMapV2): AnchorMetadata => {
  const recoveryPageEntry = rawMetadata.find(
    ([key]) => key === RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS
  );
  if (recoveryPageEntry === undefined) {
    return {};
  }
  const stringValue = recoveryPageEntry[1];
  const recoveryPageShownTimestampMillis = Number(stringValue);
  if (isNaN(recoveryPageShownTimestampMillis)) {
    return {};
  }
  return {
    recoveryPageShownTimestampMillis,
  };
};

export class AnchorMetadataRepository {
  // The nice AnchorMetadata is exposed to the outside world, while the raw metadata is kept private.
  // We keep all the raw data to maintain other metadata fields.
  private rawMetadata: MetadataMapV2 | "loading" | "error" | "not-loaded";
  // Flag to keep track whether we need to commit the metadata to the canister.
  private updatedMetadata: boolean;

  constructor(private connection: AuthenticatedConnection) {
    this.rawMetadata = "not-loaded";
    this.updatedMetadata = false;
    // Load the metadata in the background.
    void this.loadMetadata();
  }

  /**
   * We load the metadata in the background and keep it in memory as it comes.
   */
  loadMetadata = async () => {
    this.rawMetadata = "loading";
    const identityInfo = await this.connection.getIdentityInfo();
    if ("Ok" in identityInfo) {
      this.rawMetadata = identityInfo.Ok.metadata;
    } else {
      this.rawMetadata = "error";
    }
  };

  /**
   * It waits until the metadata is loaded and then converts it to a nice AnchorMetadata object.
   *
   * @throws {Error} If the metadata is not yet loaded after METADATA_FETCH_TIMEOUT * METADATA_FETCH_RETRIES milliseconds.
   * or has failed after METADATA_FETCH_RETRIES.
   * @returns {AnchorMetadata}
   */
  getMetadata = async (): Promise<AnchorMetadata> => {
    // Wait for the metadata to load.
    for (let i = 0; i < METADATA_FETCH_RETRIES; i++) {
      if (this.rawMetadata === "loading") {
        await new Promise((resolve) =>
          setTimeout(resolve, METADATA_FETCH_TIMEOUT)
        );
      } else if (
        this.rawMetadata === "error" ||
        this.rawMetadata === "not-loaded"
      ) {
        // Retry in case of error or not loaded.
        void this.loadMetadata();
      } else {
        break;
      }
    }
    if (
      this.rawMetadata === "loading" ||
      this.rawMetadata === "error" ||
      this.rawMetadata === "not-loaded"
    ) {
      throw new Error("Metadata loading took too long");
    }
    return convertMetadata(this.rawMetadata);
  };

  /**
   * Changes the copy of the metadata in memory but doesn't commit it to the canister.
   * @param {Partial<AnchorMetadata>} partialMetadata
   */
  setPartialMetadata = async (partialMetadata: Partial<AnchorMetadata>) => {
    try {
      await this.getMetadata();
    } catch (_) {
      // If the metadata is not loaded, we can't set it.
      return;
    }
    if (
      this.rawMetadata === "loading" ||
      this.rawMetadata === "error" ||
      this.rawMetadata === "not-loaded"
    ) {
      throw new Error("Metadata loading took too long");
    }
    let updatedMetadata: MetadataMapV2 = [...this.rawMetadata];
    if (partialMetadata.recoveryPageShownTimestampMillis !== undefined) {
      this.updatedMetadata = true;
      updatedMetadata = updatedMetadata
        .filter(([key]) => key !== RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS)
        .concat([
          [
            RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
            {
              String:
                partialMetadata.recoveryPageShownTimestampMillis.toString(),
            },
          ],
        ]);
    }
    this.rawMetadata = updatedMetadata;
  };

  /**
   * Commits the metadata to the canister if needed.
   *
   * @returns {boolean} Whether the metadata was committed.
   * Instead of throwing an error, we return false if the metadata is not loaded.
   */
  commitMetadata = async (): Promise<boolean> => {
    if (
      this.rawMetadata === "loading" ||
      this.rawMetadata === "error" ||
      this.rawMetadata === "not-loaded"
    ) {
      return false;
    }
    if (this.updatedMetadata) {
      const response = await this.connection.setIdentityMetadata(
        this.rawMetadata
      );
      if ("Ok" in response) {
        this.updatedMetadata = false;
        return true;
      }
      return false;
    }
    // If there was nothing to commit, we return true.
    return true;
  };
}
