use crate::anchor_management::registration::Base64;
use crate::MINUTE_NS;
use candid::Principal;
use ic_cdk::api::time;
use internet_identity_interface::internet_identity::types::Timestamp;
use std::collections::{HashMap, VecDeque};

// Expiration for a registration flow, the same as the captcha expiry
const FLOW_EXPIRATION_NS: u64 = 5 * MINUTE_NS;

#[derive(Default)]
pub struct FlowStates {
    /// In-progress registration flows tied to the respective principal. The principal will become
    /// a temp key upon successful completion of the registration flow.
    /// Registration flows are removed on three conditions:
    /// * The registration flow is completed successfully
    /// * Once the flow expires
    /// * The Internet Identity canister is upgraded
    ///
    /// The total number of flow states is limited by the registration rate limit.
    registration_flows: HashMap<Principal, RegistrationFlowState>,

    /// Deque to efficiently prune expired registration flows
    ///
    /// The total number of flow state expirations is limited by the registration rate limit.
    expirations: VecDeque<FlowExpiration>,
}

/// State of a registration flow. The state expresses which action is expected next.
#[derive(Debug, Clone)]
pub enum RegistrationFlowState {
    /// Client is expected to supply the `captcha_solution` using
    /// `check_captcha`.
    CheckCaptcha {
        flow_created_timestamp_ns: u64,
        captcha_solution: String,
        captcha_png_base64: Base64,
    },
    /// Client is expected to finish the registration (supply an [authn_method](internet_identity_interface::internet_identity::types::AuthnMethodData))
    /// using `identity_registration_finish`.
    FinishRegistration { flow_created_timestamp_ns: u64 },
}

impl RegistrationFlowState {
    pub fn created_at_timestamp_ns(&self) -> u64 {
        match self {
            RegistrationFlowState::CheckCaptcha {
                flow_created_timestamp_ns,
                ..
            }
            | RegistrationFlowState::FinishRegistration {
                flow_created_timestamp_ns,
            } => *flow_created_timestamp_ns,
        }
    }
}

impl FlowStates {
    pub fn new_registration_flow(
        &mut self,
        principal: Principal,
        flow_state: RegistrationFlowState,
    ) -> Result<(), String> {
        self.prune_expired_flows();
        if self.registration_flows.contains_key(&principal) {
            return Err("already exists".to_string());
        }

        self.expirations.push_back(FlowExpiration {
            principal,
            expiration: flow_state.created_at_timestamp_ns() + FLOW_EXPIRATION_NS,
        });

        self.registration_flows.insert(principal, flow_state);
        Ok(())
    }

    pub fn update_registration_flow(
        &mut self,
        principal: Principal,
        flow_state: RegistrationFlowState,
    ) -> Result<(), String> {
        self.prune_expired_flows();
        if self.registration_flows.remove(&principal).is_none() {
            return Err("no flow_state to update".to_string());
        }
        self.registration_flows.insert(principal, flow_state);
        Ok(())
    }

    pub fn remove_registration_flow(&mut self, principal: &Principal) {
        self.prune_expired_flows();
        self.registration_flows.remove(principal);
    }

    pub fn registration_flow_state(&self, principal: &Principal) -> Option<RegistrationFlowState> {
        self.registration_flows
            .get(principal)
            // filter out expired flow
            .filter(|flow_state| flow_state.created_at_timestamp_ns() + FLOW_EXPIRATION_NS > time())
            .cloned()
    }

    fn prune_expired_flows(&mut self) {
        const MAX_TO_PRUNE: usize = 100;

        let now = time();
        for _ in 0..MAX_TO_PRUNE {
            // The expirations are sorted by expiration time because the expiration is constant
            // and time() is monotonic
            // -> we can stop pruning once we reach a flow that is not expired
            let Some(expiration) = self.expirations.front() else {
                break;
            };

            if expiration.expiration > now {
                break;
            }
            self.registration_flows.remove(&expiration.principal);
            self.expirations.pop_front();
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct FlowExpiration {
    /// Principal which the flow is tied to.
    pub principal: Principal,
    /// The expiration timestamp of the temp key
    pub expiration: Timestamp,
}
