import { IdentityBackground } from "$src/components/identityCard";
import backgroundImage from "$src/components/identityCard.png";
import { ImageMetadata } from "astro";

export const identityBackground = IdentityBackground.getSingleton(
  // When running the build, this is also built but not in an Astro environment
  (backgroundImage as unknown as ImageMetadata).src
);
