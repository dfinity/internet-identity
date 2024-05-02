use crate::storage::anchor::DomainActivity;
use crate::{IC0_APP_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
#[derive(Eq, PartialEq)]
pub enum IIDomain {
    Ic0App,
    InternetComputerOrg,
}

impl IIDomain {
    pub fn is_same_domain(&self, activity: &DomainActivity) -> bool {
        match self {
            IIDomain::Ic0App => matches!(activity, DomainActivity::Ic0App),
            IIDomain::InternetComputerOrg => {
                matches!(activity, DomainActivity::InternetComputerOrg)
            }
        }
    }

    pub fn other_ii_domain(&self) -> IIDomain {
        match self {
            IIDomain::Ic0App => IIDomain::InternetComputerOrg,
            IIDomain::InternetComputerOrg => IIDomain::Ic0App,
        }
    }
}

impl TryFrom<&str> for IIDomain {
    type Error = ();

    fn try_from(origin: &str) -> Result<Self, Self::Error> {
        match origin {
            IC0_APP_ORIGIN => Ok(IIDomain::Ic0App),
            INTERNETCOMPUTER_ORG_ORIGIN => Ok(IIDomain::InternetComputerOrg),
            _ => Err(()),
        }
    }
}
