import { parseMessage } from "$lib/components/locale/utils";

describe("parseMessage", () => {
  it.each([
    {
      case: "with tag",
      message: "Click <0>here</0> to upgrade",
      expected: [
        {
          text: "Click ",
          type: "text",
        },
        {
          children: [
            {
              text: "here",
              type: "text",
            },
          ],
          index: 0,
          type: "node",
        },
        {
          text: " to upgrade",
          type: "text",
        },
      ],
    },
    {
      case: "with self closing tag",
      message: "Hello<0/>World",
      expected: [
        {
          text: "Hello",
          type: "text",
        },
        {
          children: [],
          index: 0,
          type: "node",
        },
        {
          text: "World",
          type: "text",
        },
      ],
    },
    {
      case: "with nested tags",
      message: "To continue, <1>please <0>confirm</0></1> your choice.",
      expected: [
        {
          text: "To continue, ",
          type: "text",
        },
        {
          children: [
            {
              text: "please ",
              type: "text",
            },
            {
              children: [
                {
                  text: "confirm",
                  type: "text",
                },
              ],
              index: 0,
              type: "node",
            },
          ],
          index: 1,
          type: "node",
        },
        {
          text: " your choice.",
          type: "text",
        },
      ],
    },
  ])("should parse message $case", ({ message, expected }) => {
    expect(parseMessage(message)).toEqual(expected);
  });
});
