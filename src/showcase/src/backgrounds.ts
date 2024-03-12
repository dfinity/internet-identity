import { IdentityBackground } from "$src/components/identityCard";
import { ImageMetadata } from "astro";
import backgroundImage from "./assets/identityCard.png";

export const identityBackground = IdentityBackground.getSingleton(
  // When running the build, this is also build but not in an Astro envioronment
  (backgroundImage as unknown as ImageMetadata).src
);
