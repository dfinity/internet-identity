//! This module contains the various consent messages that is displayed to the user when they are asked to consent to the issuance of a credential.

use crate::verify_credential_spec;
use crate::SupportedCredentialType;
use ic_verifiable_credentials::issuer_api::{
    CredentialSpec, Icrc21ConsentInfo, Icrc21ConsentPreferences, Icrc21Error, Icrc21ErrorInfo,
};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use strfmt::strfmt;
use SupportedLanguage::{English, German};

const EMPLOYMENT_VC_DESCRIPTION_EN: &str = r###"# {employer} Employment Credential

Credential that states that the holder is employed by the {employer} at the time of issuance."###;
const EMPLOYMENT_VC_DESCRIPTION_DE: &str = r###"# Beschäftigungsausweis {employer}

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin zum Zeitpunkt der Austellung bei der {employer} beschäftigt ist."###;
const DEGREE_VC_DESCRIPTION_EN: &str = r###"# Bachelor of Engineering, {institute}

Credential that states that the holder has a degree in engineering from the {institute}."###;
const DEGREE_VC_DESCRIPTION_DE: &str = r###"# Bachelor of Engineering, {institute}

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin einen Bachelorabschluss in einer Ingenieurwissenschaft des {institute} besitzt."###;

const ADULT_VC_DESCRIPTION_EN: &str = r###"# Verified Adult

Credential that states that the holder's age is at least {minAge} years."###;
const ADULT_VC_DESCRIPTION_DE: &str = r###"# Erwachsene Person

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin mindestens {minAge} Jahre alt ist."###;

lazy_static! {
    static ref CONSENT_MESSAGE_TEMPLATES: HashMap<(CredentialTemplateType, SupportedLanguage), &'static str> =
        HashMap::from([
            (
                (CredentialTemplateType::VerifiedEmployee, English),
                EMPLOYMENT_VC_DESCRIPTION_EN
            ),
            (
                (CredentialTemplateType::VerifiedEmployee, German),
                EMPLOYMENT_VC_DESCRIPTION_DE
            ),
            (
                (CredentialTemplateType::UniversityDegree, English),
                DEGREE_VC_DESCRIPTION_EN
            ),
            (
                (CredentialTemplateType::UniversityDegree, German),
                DEGREE_VC_DESCRIPTION_DE
            ),
            (
                (CredentialTemplateType::VerifiedAdult, English),
                ADULT_VC_DESCRIPTION_EN
            ),
            (
                (CredentialTemplateType::VerifiedAdult, German),
                ADULT_VC_DESCRIPTION_DE
            )
        ]);
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum CredentialTemplateType {
    VerifiedEmployee,
    UniversityDegree,
    VerifiedAdult,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum SupportedLanguage {
    English,
    German,
}

impl From<&SupportedCredentialType> for CredentialTemplateType {
    fn from(value: &SupportedCredentialType) -> Self {
        match value {
            SupportedCredentialType::VerifiedEmployee(_) => {
                CredentialTemplateType::VerifiedEmployee
            }
            SupportedCredentialType::UniversityDegree(_) => {
                CredentialTemplateType::UniversityDegree
            }
            SupportedCredentialType::VerifiedAdult(_) => CredentialTemplateType::VerifiedAdult,
        }
    }
}

impl SupportedCredentialType {
    /// Re-expands a known credential type back into its parameters, which can be used for consent message templating.
    fn to_param_tuple(&self) -> (String, String) {
        match self {
            SupportedCredentialType::VerifiedEmployee(employer) => {
                ("employer".to_string(), employer.to_string())
            }
            SupportedCredentialType::UniversityDegree(institute) => {
                ("institute".to_string(), institute.to_string())
            }
            SupportedCredentialType::VerifiedAdult(min_age) => {
                ("minAge".to_string(), format!("{min_age}").to_string())
            }
        }
    }
}

impl From<Icrc21ConsentPreferences> for SupportedLanguage {
    fn from(value: Icrc21ConsentPreferences) -> Self {
        match &value.language.to_lowercase()[..2] {
            "de" => German,
            _ => English, // english is also the fallback
        }
    }
}

impl Display for SupportedLanguage {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            English => write!(f, "en"),
            German => write!(f, "de"),
        }
    }
}

pub fn get_vc_consent_message(
    credential_spec: &CredentialSpec,
    language: &SupportedLanguage,
) -> Result<Icrc21ConsentInfo, Icrc21Error> {
    render_consent_message(credential_spec, language).map(|message| Icrc21ConsentInfo {
        consent_message: message,
        language: format!("{language}"),
    })
}

fn render_consent_message(
    credential_spec: &CredentialSpec,
    language: &SupportedLanguage,
) -> Result<String, Icrc21Error> {
    let credential_type = match verify_credential_spec(credential_spec) {
        Ok(credential_type) => credential_type,
        Err(err) => {
            return Err(Icrc21Error::UnsupportedCanisterCall(Icrc21ErrorInfo {
                description: err,
            }));
        }
    };
    let template = CONSENT_MESSAGE_TEMPLATES
        .get(&(
            CredentialTemplateType::from(&credential_type),
            language.clone(),
        ))
        .ok_or(Icrc21Error::ConsentMessageUnavailable(Icrc21ErrorInfo {
            description: "Consent message template not found".to_string(),
        }))?;

    strfmt(template, &HashMap::from([credential_type.to_param_tuple()])).map_err(|e| {
        Icrc21Error::ConsentMessageUnavailable(Icrc21ErrorInfo {
            description: e.to_string(),
        })
    })
}
