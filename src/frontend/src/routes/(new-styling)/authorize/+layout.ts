export const load = ({ url }) => {
  return {
    rpc: url.searchParams.has("rpc"),
  };
};
