use crate::state;
use crate::state::RateLimitState;
use ic_cdk::api::time;
use ic_cdk::trap;
use internet_identity_interface::internet_identity::types::RateLimitConfig;
use std::cmp::min;

/// Processes the registration rate limit:
///   1. Check if the rate limit is enabled
///   2. Initialize / update the token count
///   3. Verify that the current call is not rate limited
///
/// The rate limit is based on `tokens`. Each call uses one token. Tokens replenish over time,
/// every `time_per_token_ns` a new token is added. If tokens is 0 no further calls are allowed until
/// tokens have replenished.
/// There is a maximum of `max_tokens` tokens, when reached the tokens not increase any further.
/// This is the maximum number of calls that can be handled in a burst.
pub fn process_rate_limit() {
    let Some(config) = state::persistent_state(|ps| ps.registration_rate_limit.clone()) else {
        // rate limit disabled -> nothing to do
        return;
    };

    state::registration_rate_limit_mut(|state_opt| {
        let state = if let Some(state) = state_opt {
            add_tokens(state, &config);
            state
        } else {
            // initialize new
            *state_opt = Some(RateLimitState {
                tokens: config.max_tokens,
                token_timestamp: time(),
            });
            state_opt.as_mut().unwrap()
        };

        // deduct a token for the current call
        if state.tokens > 0 {
            state.tokens -= 1;
        } else {
            trap("rate limit reached, try again later");
        }
    })
}

/// Adds new tokens to the rate limit state according to the time past since the last update.
/// To avoid floating point computation, the state is only updated if at least enough time has passed
/// to add one full token and the `token_timestamp` refers to the last timestamp that has ben accounted
/// for in the current token count.
///
/// I.e. if the time passed would allow to add 1.7 tokens to the state, only 1 token is added and
/// the `token_timestamp` is increased by 1*`time_per_token_ns`.
fn add_tokens(state: &mut RateLimitState, config: &RateLimitConfig) {
    let new_tokens = (time() - state.token_timestamp) / config.time_per_token_ns;
    if new_tokens > 0 {
        // The number of tokens is capped otherwise tokens might accumulate
        state.tokens = min(config.max_tokens, state.tokens + new_tokens);
        state.token_timestamp += config.time_per_token_ns * new_tokens;
    }
}
