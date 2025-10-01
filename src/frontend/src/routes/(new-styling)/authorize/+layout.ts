export const load = ({ url }) => {
  return { legacyProtocol: url.searchParams.has("legacyProtocol") };
};
