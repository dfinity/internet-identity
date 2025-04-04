import type { MetadataMapV2 } from "$lib/generated/internet_identity_types";
import { isValidKey, unknownToString } from "$lib/utils/utils";

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
    partialIdentityMetadata,
  ).map(([key, value]) => {
    if (typeof value === "number") {
      return [key, { String: value.toString() }] as [
        string,
        { String: string },
      ];
    }
    return [key, { String: value as string }] as [string, { String: string }];
  });

  // Update or add entries in metadataMap
  const updatedMetadataMap: MetadataMapV2 = metadataMap.map(([key, value]) => {
    const updatedEntry = identityMetadataEntries.find(
      ([identityKey]) => identityKey === key,
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
  // Flag to keep track whether we need to commit the metadata to the canister.
  private updatedMetadata: boolean;
  private metadata: Promise<MetadataMapV2>;
  private readonly getter: MetadataGetter;
  private readonly setter: MetadataSetter;

  static init = ({
    getter,
    setter,
  }: {
    getter: MetadataGetter;
    setter: MetadataSetter;
  }) => {
    // Load the metadata in the background.
    return new IdentityMetadataRepository({ getter, setter });
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
    this.metadata = this.getter();
    this.updatedMetadata = false;
  }

  /**
   * Returns the metadata transformed to `IdentityMetadata`.
   *
   * It returns `undefined` if the metadata is not loaded.
   *
   * @returns {IdentityMetadata | undefined}
   */
  getMetadata = async (): Promise<IdentityMetadata | undefined> => {
    try {
      return convertMetadata(await this.metadata);
    } catch (e) {
      console.warn(
        "Failed to load identity metadata: ",
        unknownToString(e, "unknown error"),
      );
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
   */
  updateMetadata = (partialMetadata: Partial<IdentityMetadata>) => {
    try {
      this.metadata = this.metadata.then((metadataMap) =>
        updateMetadataMapV2({
          metadataMap: metadataMap,
          partialIdentityMetadata: partialMetadata,
        }),
      );
      this.updatedMetadata = true;
    } catch (e) {
      console.warn(
        "Error updating identity metadata: ",
        unknownToString(e, "unknown error"),
      );
    }
  };

  /**
   * Commits the metadata to the canister if needed.
   *
   * @returns {boolean} Whether the metadata was committed or there was nothing to commit.
   * `true` if the metadata was committed or there was nothing to commit.
   * `false` if there was an error committing the metadata.
   */
  commitMetadata = async (): Promise<boolean> => {
    if (this.updatedMetadata) {
      try {
        await this.setter(await this.metadata);
        // after commiting metadata is no longer 'updated'
        this.updatedMetadata = false;
        return true;
      } catch (error) {
        console.warn(
          "Error committing identity metadata: ",
          unknownToString(error, "unknown error"),
        );
        return false;
      }
    }
    // If there was nothing to commit, we return true.
    return true;
  };
}
