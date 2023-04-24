const path = require("path");

/**
 * Jest cannot process images imported within the code. This loads their path as modules for test purpose.
 *
 * e.g.
 * import loaderUrl from "../../assets/loader.webp";
 */
module.exports = {
  process(sourceText, sourcePath, options) {
    return {
      code: `module.exports = ${JSON.stringify(path.basename(sourcePath))};`,
    };
  },
  getCacheKey() {
    return "img-transform";
  },
};
