import * as universal from '../entries/pages/_layout.js';

export const index = 0;
let component_cache;
export const component = async () => component_cache ??= (await import('../entries/pages/_layout.svelte.js')).default;
export { universal };
export const universal_id = "src/routes/+layout.js";
export const imports = ["_app/immutable/nodes/0.B793-XdN.js","_app/immutable/chunks/pUpZq1fU.js","_app/immutable/chunks/B39EYcLZ.js","_app/immutable/chunks/CLiMtd86.js","_app/immutable/chunks/D52fYbM_.js","_app/immutable/chunks/3-1D8cKG.js"];
export const stylesheets = ["_app/immutable/assets/0.DKYMp9kH.css"];
export const fonts = [];
