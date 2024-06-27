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

export class MetadataNotLoadedError extends Error {}
export class MetadataNotCommittedError extends Error {}

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
   * Load the metadata in the instance variable `rawMetadata`.
   *
   * This method won't throw an error if the metadata can't be loaded.
   * Instead, it will set the instance variable `rawMetadata` to "error".
   *
   * @returns {Promise<void>} In case a client wants to wait for the metadata to be loaded.
   */
  loadMetadata = async (): Promise<void> => {
    this.rawMetadata = "loading";
    const identityInfo = await this.connection.getIdentityInfo();
    if ("Ok" in identityInfo) {
      this.updatedMetadata = false;
      this.rawMetadata = identityInfo.Ok.metadata;
    } else {
      this.rawMetadata = "error";
    }
  };

  /**
   * It waits until the metadata is loaded and then converts it to a nice AnchorMetadata object.
   *
   * @throws {MetadataNotLoadedError} If the metadata is not yet loaded after METADATA_FETCH_TIMEOUT * METADATA_FETCH_RETRIES milliseconds.
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
      throw new MetadataNotLoadedError("Metadata couldn't be loaded.");
    }
    return convertMetadata(this.rawMetadata);
  };

  /**
   * Changes the metadata in memory but doesn't commit it to the canister.
   *
   * The metadata passed will be merged with the existing metadata. Same keys will be overwritten.
   *
   * If the metadata is not loaded yet, it will try to load it
   * If loading the metadata fails, it will throw an error.
   *
   * @param {Partial<AnchorMetadata>} partialMetadata
   * @throws {MetadataNotLoadedError} If the metadata can't be loaded.
   * @returns {Promise<void>} To indicate that the metadata has been set.
   */
  setPartialMetadata = async (
    partialMetadata: Partial<AnchorMetadata>
  ): Promise<void> => {
    await this.getMetadata();
    if (
      this.rawMetadata === "loading" ||
      this.rawMetadata === "error" ||
      this.rawMetadata === "not-loaded"
    ) {
      throw new MetadataNotLoadedError("Metadata couldn't be loaded.");
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
   * @throws {MetadataNotLoadedError} If the metadata can't be committed because it's not loaded yet or there was an error.
   * @throws {MetadataNotCommittedError} If the metadata can't be committed because there was an error connecting with the canister.
   * @returns {boolean} Whether the metadata was committed.
   */
  commitMetadata = async (): Promise<boolean> => {
    if (
      this.rawMetadata === "loading" ||
      this.rawMetadata === "error" ||
      this.rawMetadata === "not-loaded"
    ) {
      throw new MetadataNotLoadedError("Metadata couldn't be loaded.");
    }
    if (this.updatedMetadata) {
      const response = await this.connection.setIdentityMetadata(
        this.rawMetadata
      );
      if ("Ok" in response) {
        this.updatedMetadata = false;
        return true;
      }
      throw new MetadataNotCommittedError(JSON.stringify(response.Err));
    }
    // If there was nothing to commit, we return true.
    return true;
  };
}
