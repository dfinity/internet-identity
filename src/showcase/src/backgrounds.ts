import { IdentityBackground } from "$src/components/identityCard";
import backgroundImage from "$src/components/identityCard.png";
import { ImageMetadata } from "astro";

export const identityBackground = IdentityBackground.getSingleton(
  // When running the build, this is also build but not in an Astro envioronment
  (backgroundImage as unknown as ImageMetadata).src
);
