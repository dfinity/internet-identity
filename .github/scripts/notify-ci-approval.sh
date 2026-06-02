#!/usr/bin/env bash
# Notify Slack when fork-PR workflow runs are stuck in action_required.
#
# Requires: gh (authenticated), curl, jq
# Env:      SLACK_CI_APPROVAL_WEBHOOK_URL (optional — exits cleanly if unset)
#           GITHUB_REPOSITORY (e.g. dfinity/internet-identity)
set -euo pipefail

REPO="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY must be set}"
MARKER_PREFIX="<!-- ci-approval-slack:"

if [[ -z "${SLACK_CI_APPROVAL_WEBHOOK_URL:-}" ]]; then
  echo "SLACK_CI_APPROVAL_WEBHOOK_URL is not set — skipping notification."
  exit 0
fi

# Fetch all action_required workflow runs.
runs=$(gh api "repos/${REPO}/actions/runs?status=action_required&per_page=100" \
  --jq '.workflow_runs')

if [[ -z "$runs" || "$runs" == "null" || "$runs" == "[]" ]]; then
  echo "No action_required runs found."
  exit 0
fi

# Group runs by the originating PR's head SHA (source branch HEAD, not
# the merge commit). We key on (pr_number, head_sha) to avoid spurious
# re-notifications when the base branch advances.
#
# Each run has .pull_requests[] — we take the first one. Runs without a
# PR association (e.g. branch pushes) are skipped.
#
# Output: one JSON object per unique (pr_number, head_sha) with fields:
#   pr_number, head_sha, head_short, run_count, pr_url, author, title, approve_url
grouped=$(echo "$runs" | jq -r '
  [ .[]
    | select(.pull_requests | length > 0)
    | {
        pr_number: .pull_requests[0].number,
        head_sha:  .head_sha,
        head_short: (.head_sha[:7]),
        repo_url:  (.repository.html_url // ""),
        run_html:  .html_url,
        actor:     (.actor.login // "unknown"),
        pr_url:    (.pull_requests[0].url // "")
      }
  ]
  | group_by(.pr_number + ":" + .head_sha)
  | [ .[] | {
        pr_number:  .[0].pr_number,
        head_sha:   .[0].head_sha,
        head_short: .[0].head_short,
        run_count:  length,
        actor:      .[0].actor,
        repo_url:   .[0].repo_url,
        run_html:   .[0].run_html,
        pr_url:     .[0].pr_url
      }
    ]
')

if [[ -z "$grouped" || "$grouped" == "null" || "$grouped" == "[]" ]]; then
  echo "No action_required runs linked to open PRs."
  exit 0
fi

# For each unique (PR, head SHA), check dedup marker and notify if new.
notified=0
echo "$grouped" | jq -c '.[]' | while IFS= read -r entry; do
  pr_number=$(echo "$entry" | jq -r '.pr_number')
  head_sha=$(echo "$entry"  | jq -r '.head_sha')
  head_short=$(echo "$entry" | jq -r '.head_short')
  run_count=$(echo "$entry"  | jq -r '.run_count')
  actor=$(echo "$entry"      | jq -r '.actor')
  run_html=$(echo "$entry"   | jq -r '.run_html')
  pr_rest_url=$(echo "$entry" | jq -r '.pr_url')

  # Fetch PR details (title, html_url) from the REST API.
  pr_data=$(gh api "repos/${REPO}/pulls/${pr_number}" --jq '{title: .title, html_url: .html_url, state: .state}' 2>/dev/null || echo '{}')
  pr_title=$(echo "$pr_data" | jq -r '.title // "PR #'"${pr_number}"'"')
  pr_html=$(echo "$pr_data"  | jq -r '.html_url // "https://github.com/'"${REPO}"'/pull/'"${pr_number}"'"')
  pr_state=$(echo "$pr_data" | jq -r '.state // "unknown"')

  # Skip closed PRs — their runs are stale.
  if [[ "$pr_state" != "open" ]]; then
    echo "PR #${pr_number} is ${pr_state}, skipping."
    continue
  fi

  # Check dedup: look for our marker comment on this PR with this head SHA.
  marker="${MARKER_PREFIX} head=${head_sha} -->"
  existing=$(gh api "repos/${REPO}/issues/${pr_number}/comments?per_page=100" \
    --jq '[.[] | select(.body | contains("'"${MARKER_PREFIX}"'")) | .body] | last // ""' 2>/dev/null || echo "")

  if echo "$existing" | grep -qF "head=${head_sha}"; then
    echo "PR #${pr_number} head=${head_short}: already notified, skipping."
    continue
  fi

  # Build the approval-UI link (Actions tab filtered to the PR).
  approve_url="https://github.com/${REPO}/actions?query=event%3Apull_request_target+is%3Aaction_required"

  # Post Slack notification.
  slack_text=":warning: *CI approval needed* for <${pr_html}|${pr_title}> by \`${actor}\`\n:point_right: Head: \`${head_short}\` · ${run_count} pending run(s)\n<${approve_url}|Approve workflow runs>"

  response=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
    -H 'Content-Type: application/json' \
    --data "$(jq -n --arg text "$slack_text" '{text: $text}')" \
    "$SLACK_CI_APPROVAL_WEBHOOK_URL")

  if [[ "$response" == "200" ]]; then
    echo "PR #${pr_number} head=${head_short}: Slack notification sent."
  else
    echo "PR #${pr_number} head=${head_short}: Slack returned HTTP ${response}."
  fi

  # Post dedup marker on the PR as an HTML comment (invisible in rendered markdown).
  marker_body="${marker}
:robot_face: Slack notification sent for CI approval on head \`${head_short}\` (${run_count} pending run(s)).

_Automated by [ci-approval-slack-notifier](https://github.com/${REPO}/actions/workflows/ci-approval-slack-notifier.yml)._"

  gh api "repos/${REPO}/issues/${pr_number}/comments" \
    -f body="$marker_body" --silent 2>/dev/null || \
    echo "PR #${pr_number}: failed to post dedup marker (non-fatal)."

  notified=$((notified + 1))
done

echo "Done. Notifications sent this run: ${notified:-0}."
