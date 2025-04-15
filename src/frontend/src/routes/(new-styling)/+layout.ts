export function load({ url }: { url: URL }) {
  return {
    url: url.pathname,
  };
}
