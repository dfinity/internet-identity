import { MetadataMapV2 } from "$generated/internet_identity_types";

export type IdentityMetadata = {
  recoveryPageShownTimestampMillis?: number;
};

export const RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS =
  "recoveryPageShownTimestampMillis";

const convertMetadata = (rawMetadata: MetadataMapV2): IdentityMetadata => {
  const recoveryPageEntry = rawMetadata.find(
    ([key]) => key === RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS
  );
  if (recoveryPageEntry === undefined) {
    return {};
  }
  const stringValue = recoveryPageEntry[1];
  if ("String" in stringValue) {
    const recoveryPageShownTimestampMillis = Number(stringValue.String);
    if (isNaN(recoveryPageShownTimestampMillis)) {
      return {};
    }
    return {
      recoveryPageShownTimestampMillis,
    };
  }
  return {};
};

type MetadataGetter = () => Promise<MetadataMapV2>;
type MetadataSetter = (metadata: MetadataMapV2) => Promise<void>;
type RawMetadataState = MetadataMapV2 | "loading" | "error" | "not-loaded";

/**
 * Class to manage the metadata of the identity and interact with the canister.
 *
 * We decided to not throw any errors because this is non-critical for the application
 * and we don't want to disrupt user flows if there is an error with the metadata.
 *
 * This class loads the metadata in the background when it's created.
 * It can then be read and updated in memory.
 * The metadata needs to be committed to the canister with the `commitMetadata` method to persist the changes.
 */
export class IdentityMetadataRepository {
  // The nice IdentityMetadata is exposed to the outside world, while the raw metadata is kept private.
  // We keep all the raw data to maintain other metadata fields.
  private rawMetadata: RawMetadataState;
  // Flag to keep track whether we need to commit the metadata to the canister.
  private updatedMetadata: boolean;
  private readonly getter: MetadataGetter;
  private readonly setter: MetadataSetter;

  static init = ({
    getter,
    setter,
  }: {
    getter: MetadataGetter;
    setter: MetadataSetter;
  }) => {
    const instance = new IdentityMetadataRepository({ getter, setter });
    // Load the metadata in the background.
    void instance.loadMetadata();
    return instance;
  };

  private constructor({
    getter,
    setter,
  }: {
    getter: MetadataGetter;
    setter: MetadataSetter;
  }) {
    this.getter = getter;
    this.setter = setter;
    this.rawMetadata = "not-loaded";
    this.updatedMetadata = false;
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
    try {
      this.updatedMetadata = false;
      this.rawMetadata = await this.getter();
    } catch (error) {
      // Do not throw the error because this is not critical for the application.
      this.rawMetadata = "error";
      console.warn("Error loading metadata", error);
      return;
    }
  };

  private metadataIsLoaded = (
    metadata: RawMetadataState
  ): metadata is MetadataMapV2 => {
    return (
      metadata !== "loading" &&
      metadata !== "error" &&
      metadata !== "not-loaded"
    );
  };

  /**
   * Waits a maximum of 10 seconds for the metadata to be loaded.
   *
   * It doesn't throw an error if the metadata is not loaded.
   */
  private waitUntilMetadataIsLoaded = async (): Promise<void> => {
    let currentWait = 0;
    const MAX_WAIT_MILLIS = 10_000;
    const ONE_WAIT_MILLIS = 1_000;
    while (this.rawMetadata === "loading" || currentWait < MAX_WAIT_MILLIS) {
      await new Promise((resolve) => setTimeout(resolve, 100));
      currentWait += ONE_WAIT_MILLIS;
    }
  };

  /**
   * Returns the metadata transformed to `IdentityMetadata`.
   *
   * It returns `undefined` if the metadata is not loaded.
   *
   * @returns {IdentityMetadata | undefined}
   */
  getMetadata = async (): Promise<IdentityMetadata | undefined> => {
    await this.waitUntilMetadataIsLoaded();
    if (this.metadataIsLoaded(this.rawMetadata)) {
      return convertMetadata(this.rawMetadata);
    }
    return undefined;
  };

  /**
   * Changes the metadata in memory but doesn't commit it to the canister.
   *
   * At the moment, this function only supports changing the `recoveryPageShownTimestampMillis` field.
   *
   * The metadata passed will be merged with the existing metadata. Same keys will be overwritten.
   *
   * We consider the metadata to not be crucial for the application.
   * Therefore, we don't want to disrupt user flows if there is an error with the metadata.
   *
   * @param {Partial<IdentityMetadata>} partialMetadata
   * @returns {Promise<void>} To indicate that the metadata has been set.
   */
  updateMetadata = async (
    partialMetadata: Partial<IdentityMetadata>
  ): Promise<void> => {
    await this.waitUntilMetadataIsLoaded();
    if (this.metadataIsLoaded(this.rawMetadata)) {
      let updatedMetadata: MetadataMapV2 = [...this.rawMetadata];
      this.updatedMetadata = true;
      updatedMetadata = updatedMetadata
        .filter(([key]) => key !== RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS)
        .concat([
          [
            RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
            {
              String: String(partialMetadata.recoveryPageShownTimestampMillis),
            },
          ],
        ]);
      this.rawMetadata = updatedMetadata;
    }
    // Do nothing if the metadata is not loaded.
  };

  /**
   * Commits the metadata to the canister if needed.
   *
   * @returns {boolean} Whether the metadata was committed or there was nothing to commit.
   * `true` if the metadata was committed or there was nothing to commit.
   * `false` if there was an error committing the metadata.
   */
  commitMetadata = async (): Promise<boolean> => {
    if (this.metadataIsLoaded(this.rawMetadata) && this.updatedMetadata) {
      try {
        await this.setter(this.rawMetadata);
        return true;
      } catch (error) {
        console.warn("Error committing metadata", error);
        return false;
      }
    }
    // If there was nothing to commit, we return true.
    return true;
  };
}
