//! This module contains the various consent messages that is displayed to the user when they are asked to consent to the issuance of a credential.

use crate::SupportedCredentialType;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use strfmt::strfmt;
use vc_util::issuer_api::{
    Icrc21ConsentInfo, Icrc21ConsentMessageResponse, Icrc21ConsentPreferences, Icrc21Error,
    Icrc21ErrorInfo,
};
use SupportedCredentialType::{UniversityDegreeCredential, VerifiedEmployee};
use SupportedLanguage::{English, German};

const EMPLOYMENT_VC_DESCRIPTION_EN: &str = r###"# {employer} Employment Credential

Credential that states that the holder is employed by the {employer} at the time of issuance."###;
const EMPLOYMENT_VC_DESCRIPTION_DE: &str = r###"# Beschäftigungsausweis {employer}

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin zum Zeitpunkt der Austellung bei der {employer} beschäftigt ist."###;
const DEGREE_VC_DESCRIPTION_EN: &str = r###"# Bachelor of Engineering, {institute}

Credential that states that the holder has a degree in engineering from the {institute}."###;
const DEGREE_VC_DESCRIPTION_DE: &str = r###"# Bachelor of Engineering, {institute}

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin einen Bachelorabschluss in einer Ingenieurwissenschaft des {institute} besitzt."###;

pub enum SupportedLanguage {
    English,
    German,
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
    credential_type: &SupportedCredentialType,
    language: &SupportedLanguage,
) -> Icrc21ConsentMessageResponse {
    let template_result = match (credential_type, language) {
        (VerifiedEmployee(employer), English) => employment_consent_msg_eng(employer),
        (VerifiedEmployee(employer), German) => employment_consent_msg_de(employer),
        (UniversityDegreeCredential(institute), English) => degree_consent_msg_eng(institute),
        (UniversityDegreeCredential(institute), German) => degree_consent_msg_de(institute),
    };

    match template_result {
        Ok(message) => Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
            consent_message: message,
            language: format!("{}", language),
        }),
        Err(err) => Icrc21ConsentMessageResponse::Err(Icrc21Error::GenericError(err)),
    }
}

fn employment_consent_msg_eng(employer: &str) -> Result<String, Icrc21ErrorInfo> {
    strfmt(
        EMPLOYMENT_VC_DESCRIPTION_EN,
        &HashMap::from([("employer".to_string(), employer)]),
    )
    .map_err(|e| Icrc21ErrorInfo {
        error_code: 0,
        description: e.to_string(),
    })
}

fn employment_consent_msg_de(employer: &str) -> Result<String, Icrc21ErrorInfo> {
    strfmt(
        EMPLOYMENT_VC_DESCRIPTION_DE,
        &HashMap::from([("employer".to_string(), employer)]),
    )
    .map_err(|e| Icrc21ErrorInfo {
        error_code: 0,
        description: e.to_string(),
    })
}

fn degree_consent_msg_eng(institute: &str) -> Result<String, Icrc21ErrorInfo> {
    strfmt(
        DEGREE_VC_DESCRIPTION_EN,
        &HashMap::from([("institute".to_string(), institute)]),
    )
    .map_err(|e| Icrc21ErrorInfo {
        error_code: 0,
        description: e.to_string(),
    })
}

fn degree_consent_msg_de(institute: &str) -> Result<String, Icrc21ErrorInfo> {
    strfmt(
        DEGREE_VC_DESCRIPTION_DE,
        &HashMap::from([("institute".to_string(), institute)]),
    )
    .map_err(|e| Icrc21ErrorInfo {
        error_code: 0,
        description: e.to_string(),
    })
}
