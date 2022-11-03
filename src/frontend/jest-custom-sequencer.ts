const Sequencer = require("@jest/test-sequencer").default;

/** A sequencer that ensures that ensures that the slowest tests
 * all land on different shards. */
class CustomSequencer extends Sequencer {
  shard(tests, { shardIndex, shardCount }) {
    // Tests by order of increasing duration. Update by looking at test duration.
    // Does not need to be exact.
    const testFilesByDuration = [
      "delegationTtl.test.ts",
      "misc.test.ts",
      "alternativeOrigin.test.ts",
      "recovery.test.ts",
      "register.test.ts",
    ];

    // Try to find the index (bigger is slower) in the file list, return -1
    // otherwise.
    const duration = (a) =>
      testFilesByDuration.findIndex((e) => a.path.endsWith(e));

    // Sort by duration (slowest first) and then distribute by shard index.
    return [...tests]
      .sort((a, b) => duration(b) - duration(a))
      .filter((_, index) => (index - (shardIndex - 1)) % shardCount === 0);
  }
}

module.exports = CustomSequencer;
