#!/usr/bin/env bash
# Helpers for resolving the latest *adopted* (EXECUTED) NNS proposal
# for an II canister role from local `proposal-<role>-N` git tags.
#
# Why: `make-upgrade-proposal` tags `proposal-<role>-N` at proposal
# *creation*, not adoption. The tag namespace therefore contains
# rejected/open/failed proposals too. Anything that needs "the last
# release that actually shipped" must check NNS status before
# trusting the highest-numbered tag.

# Print the NNS proposal status string ("EXECUTED" / "REJECTED" / ...).
# Exit 0 on success, 2 on transient API/network failure (so callers
# can distinguish "we know the answer" from "we couldn't tell").
# Honors $IC_API_BASE for tests.
nns_proposal_status() {
  local id="$1"
  local base="${IC_API_BASE:-https://ic-api.internetcomputer.org}"
  local body
  body=$(curl -fsS --max-time 10 "${base}/api/v3/proposals/${id}") || return 2
  local status
  status=$(echo "$body" | jq -r '.status // empty') || return 2
  [ -n "$status" ] || return 2
  echo "$status"
}

# Walk `proposal-<role>-*` tags newest→oldest and print the first whose
# NNS status is EXECUTED.
# Exit 0 + tag on stdout → found
# Exit 1                 → no EXECUTED tag (genuine empty / all
#                          rejected, OR API failures). Stderr says
#                          which, so callers can phrase prompts.
# Bounded at 20 tags scanned: the executed baseline is always within
# the most recent handful and this keeps API load trivially small.
# Args: $1 = "backend" | "frontend"
latest_executed_proposal_tag() {
  local role="$1"
  local tag id status
  local seen_transient=0 seen_success=0
  while IFS= read -r tag; do
    id="${tag#proposal-${role}-}"
    [[ "$id" =~ ^[0-9]+$ ]] && [ "$id" -ge 100000 ] || continue
    if status=$(nns_proposal_status "$id"); then
      seen_success=1
      [ "$status" = "EXECUTED" ] && { echo "$tag"; return 0; }
      echo "Skipping ${tag} (status: ${status})" >&2
    else
      seen_transient=1
      echo "Warning: could not query NNS status for ${tag}" >&2
    fi
  done < <(git tag -l "proposal-${role}-*" \
            | sed -E "s/^proposal-${role}-//" \
            | sort -n -r \
            | head -20 \
            | sed -E "s/^/proposal-${role}-/")
  if [ "$seen_transient" = "1" ]; then
    echo "No EXECUTED proposal-${role}-* tag confirmed; dashboard API was unreachable for at least one tag." >&2
  elif [ "$seen_success" = "1" ]; then
    echo "No EXECUTED proposal-${role}-* tag exists (all recent ${role} proposals were rejected/open/failed)." >&2
  fi
  return 1
}
