#!/usr/bin/env bash
# Notify Slack when fork-PR workflow runs are stuck in action_required.
#
# Requires: gh (authenticated), curl, jq
# Env:      SLACK_PRIVATE_IDENTITY_WEBHOOK_URL (optional — exits cleanly if unset)
#           GITHUB_REPOSITORY (e.g. dfinity/internet-identity)
set -euo pipefail

REPO="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY must be set}"
MARKER_PREFIX="<!-- ci-approval-slack:"

if [[ -z "${SLACK_PRIVATE_IDENTITY_WEBHOOK_URL:-}" ]]; then
  echo "SLACK_PRIVATE_IDENTITY_WEBHOOK_URL is not set — skipping notification."
  exit 0
fi

# Fetch all action_required workflow runs.
runs=$(gh api "repos/${REPO}/actions/runs?status=action_required&per_page=100" \
  --jq '.workflow_runs')

if [[ -z "$runs" || "$runs" == "null" || "$runs" == "[]" ]]; then
  echo "No action_required runs found."
  exit 0
fi

# Fork-PR runs in action_required commonly have pull_requests:[].
# Fetch open PRs to resolve head_sha → PR when the run itself lacks the link.
open_prs=$(gh api --paginate "repos/${REPO}/pulls?state=open&per_page=100" \
  --jq '.[] | {number, head_sha: .head.sha, title, html_url, author: .user.login}' \
  | jq -s '.')

# Group runs by (pr_number, head_sha). For runs with empty pull_requests[],
# fall back to matching head_sha against the open-PR list.
grouped=$(jq -n \
  --argjson runs "$runs" \
  --argjson prs "$open_prs" \
  '
  ($prs | map({key: .head_sha, value: .}) | from_entries) as $sha_map |
  [
    $runs[] |
    .head_sha as $sha |
    (if (.pull_requests | length) > 0 then
       .pull_requests[0].number
     elif $sha_map[$sha] then
       $sha_map[$sha].number
     else null end) as $pr_num |
    select($pr_num != null) |
    {
      pr_number:  $pr_num,
      head_sha:   $sha,
      head_short: ($sha[:7]),
      actor:      (.actor.login // "unknown"),
      run_html:   .html_url
    }
  ]
  | group_by("\(.pr_number):\(.head_sha)")
  | [.[] | {
      pr_number:  .[0].pr_number,
      head_sha:   .[0].head_sha,
      head_short: .[0].head_short,
      run_count:  length,
      actor:      .[0].actor,
      run_html:   .[0].run_html
    }]
  ')

if [[ -z "$grouped" || "$grouped" == "null" || "$grouped" == "[]" ]]; then
  echo "No action_required runs linked to open PRs."
  exit 0
fi

notified=0
while IFS= read -r entry; do
  pr_number=$(echo "$entry" | jq -r '.pr_number')
  head_sha=$(echo "$entry"  | jq -r '.head_sha')
  head_short=$(echo "$entry" | jq -r '.head_short')
  run_count=$(echo "$entry"  | jq -r '.run_count')
  actor=$(echo "$entry"      | jq -r '.actor')

  # Resolve PR metadata from the preloaded open-PR list.
  pr_info=$(echo "$open_prs" | jq --argjson n "$pr_number" \
    'map(select(.number == $n)) | first // null')
  if [[ -z "$pr_info" || "$pr_info" == "null" ]]; then
    echo "PR #${pr_number}: not in open-PR list, skipping."
    continue
  fi
  pr_title=$(echo "$pr_info" | jq -r '.title')
  pr_html=$(echo "$pr_info"  | jq -r '.html_url')

  # Dedup: look for our marker comment with this head SHA.
  marker="${MARKER_PREFIX} head=${head_sha} -->"
  existing=$(gh api --paginate "repos/${REPO}/issues/${pr_number}/comments" \
    --jq '.[] | select(.body | contains("'"${MARKER_PREFIX}"'")) | .body' \
    | tail -1)

  if echo "${existing:-}" | grep -qF "head=${head_sha}"; then
    echo "PR #${pr_number} head=${head_short}: already notified, skipping."
    continue
  fi

  approve_url="https://github.com/${REPO}/actions?query=event%3Apull_request_target+is%3Aaction_required"

  slack_text=":warning: *CI approval needed* for <${pr_html}|${pr_title}> by \`${actor}\`\n:point_right: Head: \`${head_short}\` · ${run_count} pending run(s)\n<${approve_url}|Approve workflow runs>"

  response=$(curl -s -o /dev/null -w "%{http_code}" -X POST \
    -H 'Content-Type: application/json' \
    --data "$(jq -n --arg text "$slack_text" '{text: $text}')" \
    "$SLACK_PRIVATE_IDENTITY_WEBHOOK_URL")

  if [[ "$response" == "200" ]]; then
    echo "PR #${pr_number} head=${head_short}: Slack notification sent."
  else
    echo "PR #${pr_number} head=${head_short}: Slack returned HTTP ${response}."
  fi

  # Post dedup marker on the PR.
  marker_body="${marker}
:robot_face: Slack notification sent for CI approval on head \`${head_short}\` (${run_count} pending run(s)).

_Automated by [ci-approval-slack-notifier](https://github.com/${REPO}/actions/workflows/ci-approval-slack-notifier.yml)._"

  gh api "repos/${REPO}/issues/${pr_number}/comments" \
    -f body="$marker_body" --silent || \
    echo "PR #${pr_number}: failed to post dedup marker (non-fatal)."

  notified=$((notified + 1))
done < <(echo "$grouped" | jq -c '.[]')

echo "Done. Notifications sent this run: ${notified}."
