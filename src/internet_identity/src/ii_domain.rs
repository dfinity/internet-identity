use crate::storage::anchor::DomainActivity;
use crate::{IC0_APP_ORIGIN, ID_AI_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
use serde::{Deserialize, Serialize};
#[derive(Deserialize, Serialize, Eq, PartialEq, Clone, Debug, Ord, PartialOrd, Hash)]
pub enum IIDomain {
    Ic0App,
    InternetComputerOrg,
    IdAi,
}

pub fn maybe_domain_to_label(domain: &Option<IIDomain>) -> &'static str {
    match domain {
        Some(IIDomain::Ic0App) => "ic0.app",
        Some(IIDomain::InternetComputerOrg) => "internetcomputer.org",
        Some(IIDomain::IdAi) => "id.ai",
        None => "other",
    }
}

impl IIDomain {
    pub fn is_same_domain(&self, activity: &DomainActivity) -> bool {
        match self {
            IIDomain::Ic0App => matches!(activity, DomainActivity::Ic0App),
            IIDomain::InternetComputerOrg => {
                matches!(activity, DomainActivity::InternetComputerOrg)
            }
            IIDomain::IdAi => matches!(activity, DomainActivity::IdAi),
        }
    }
}

impl TryFrom<&str> for IIDomain {
    type Error = ();

    fn try_from(origin: &str) -> Result<Self, Self::Error> {
        match origin {
            IC0_APP_ORIGIN => Ok(IIDomain::Ic0App),
            INTERNETCOMPUTER_ORG_ORIGIN => Ok(IIDomain::InternetComputerOrg),
            ID_AI_ORIGIN => Ok(IIDomain::IdAi),
            _ => Err(()),
        }
    }
}
