import { MetadataMapV2 } from "$generated/internet_identity_types";
import { isValidKey } from "$src/utils/utils";

export type IdentityMetadata = {
  recoveryPageShownTimestampMillis?: number;
  doNotShowRecoveryPageRequestTimestampMillis?: number;
};

export const RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS =
  "recoveryPageShownTimestampMillis";
export const DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS =
  "doNotShowRecoveryPageRequestTimestampMillis";
const NUMBER_KEYS: Array<keyof IdentityMetadata> = [
  RECOVERY_PAGE_SHOW_TIMESTAMP_MILLIS,
  DO_NOT_SHOW_RECOVERY_PAGE_REQUEST_TIMESTAMP_MILLIS,
];

const convertMetadata = (rawMetadata: MetadataMapV2): IdentityMetadata => {
  return rawMetadata.reduce((acc, [key, value]) => {
    if (isValidKey(key, NUMBER_KEYS)) {
      if (!("String" in value)) {
        return acc;
      }
      const stringValue = value.String;
      const numberValue = Number(stringValue);
      if (!isNaN(numberValue)) {
        // We need to cast the key because TS is not smart enough to
        acc[key] = numberValue;
      }
    }
    return acc;
  }, {} as IdentityMetadata);
};

const updateMetadataMapV2 = ({
  metadataMap,
  partialIdentityMetadata,
}: {
  metadataMap: MetadataMapV2;
  partialIdentityMetadata: Partial<IdentityMetadata>;
}): MetadataMapV2 => {
  // Convert the partialIdentityMetadata into the format of MetadataMapV2
  const identityMetadataEntries: MetadataMapV2 = Object.entries(
    partialIdentityMetadata
  ).map(([key, value]) => {
    if (typeof value === "number") {
      return [key, { String: value.toString() }] as [
        string,
        { String: string }
      ];
    }
    return [key, { String: value as string }] as [string, { String: string }];
  });

  // Update or add entries in metadataMap
  const updatedMetadataMap: MetadataMapV2 = metadataMap.map(([key, value]) => {
    const updatedEntry = identityMetadataEntries.find(
      ([identityKey]) => identityKey === key
    );
    return updatedEntry ? updatedEntry : [key, value];
  });

  // Add new entries that were not in the original metadataMap
  identityMetadataEntries.forEach(([identityKey, identityValue]) => {
    if (!metadataMap.some(([key]) => key === identityKey)) {
      updatedMetadataMap.push([identityKey, identityValue]);
    }
  });

  return updatedMetadataMap;
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
    while (this.rawMetadata === "loading" && currentWait < MAX_WAIT_MILLIS) {
      await new Promise((resolve) => setTimeout(resolve, ONE_WAIT_MILLIS));
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
      this.updatedMetadata = true;
      this.rawMetadata = updateMetadataMapV2({
        metadataMap: this.rawMetadata,
        partialIdentityMetadata: partialMetadata,
      });
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
