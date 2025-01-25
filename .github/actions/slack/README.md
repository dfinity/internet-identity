# Slack

A GitHub Action for sending slack notifications. For more information, see slack's [documentation](https://api.slack.com/messaging/webhooks). In particular, note that the channel and workspace are set by the webhook URL, not directly by this action.

## Usage

```yaml
name: My Action

on:
  push:

jobs:
  share-love:
    runs-on: ubuntu-latest
    steps:
      - uses: ./.github/actions/slack
        with:
          WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
          MESSAGE: "I love you guys."
```
