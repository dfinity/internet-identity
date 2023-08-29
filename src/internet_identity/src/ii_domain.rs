use crate::storage::anchor::DomainActivity;
use crate::{IC0_APP_ORIGIN, INTERNETCOMPUTER_ORG_ORIGIN};
#[derive(Eq, PartialEq)]
pub enum IIDomain {
    Ic0AppDomain,
    InternetComputerOrgDomain,
}

impl IIDomain {
    pub fn is_same_domain(&self, activity: &DomainActivity) -> bool {
        match self {
            IIDomain::Ic0AppDomain => matches!(activity, DomainActivity::Ic0App),
            IIDomain::InternetComputerOrgDomain => {
                matches!(activity, DomainActivity::InternetComputerOrg)
            }
        }
    }

    pub fn other_ii_domain(&self) -> IIDomain {
        match self {
            IIDomain::Ic0AppDomain => IIDomain::InternetComputerOrgDomain,
            IIDomain::InternetComputerOrgDomain => IIDomain::Ic0AppDomain,
        }
    }
}

impl TryFrom<&str> for IIDomain {
    type Error = ();

    fn try_from(origin: &str) -> Result<Self, Self::Error> {
        match origin {
            IC0_APP_ORIGIN => Ok(IIDomain::Ic0AppDomain),
            INTERNETCOMPUTER_ORG_ORIGIN => Ok(IIDomain::InternetComputerOrgDomain),
            _ => Err(()),
        }
    }
}
