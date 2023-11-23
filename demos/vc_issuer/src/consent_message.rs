//! This module contains the various consent messages that is displayed to the user when they are asked to consent to the issuance of a credential.

use crate::SupportedCredentialType;
use std::fmt::{Display, Formatter};
use vc_util::issuer_api::{
    Icrc21ConsentInfo, Icrc21ConsentMessageResponse, Icrc21ConsentPreferences,
};
use SupportedCredentialType::{UniversityDegreeCredential, VerifiedEmployee};
use SupportedLanguage::{English, German};

const EMPLOYMENT_VC_DESCRIPTION_EN: &str = r###"# DFINITY Foundation Employment Credential

Credential that states that the holder is employed by the DFINITY Foundation at the time of issuance."###;
const DEGREE_VC_DESCRIPTION_EN: &str = r###"# Bachelor of Engineering, DFINITY College of Engineering

Credential that states that the holder has a degree in engineering from the DFINITY College of Engineering."###;
const VALIDITY_INFO_EN: &str = "The credential is valid for 15 minutes.";
const ANONYMITY_DISCLAIMER_EN: &str = "This credential does **not** contain any additional personal information. It is issued to an ephemeral identity that is created for the sole purpose of issuing this credential.";

const EMPLOYMENT_VC_DESCRIPTION_DE: &str = r###"# Beschäftigungsausweis DFINITY Stiftung

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin zum Zeitpunkt der Austellung bei der DFINITY Stiftung beschäftigt ist."###;
const DEGREE_VC_DESCRIPTION_DE: &str = r###"# Bachelor of Engineering, DFINITY Hochschule für Ingenieurwissenschaften

Ausweis, der bestätigt, dass der Besitzer oder die Besitzerin einen Bachelorabschluss in einer Ingenieurwissenschaft der DFINITY Hochschule für Ingenieurwissenschaften besitzt."###;

const VALIDITY_INFO_DE: &str = "Dieser Ausweis ist gültig für 15 Minuten.";
const ANONYMITY_DISCLAIMER_DE: &str =
    "Dieser Ausweis enthält **keine** zusätzlichen persönlichen Daten. Er wird auf eine kurzlebige Identität lautend ausgestellt, die für den alleinigen Verwendungszweck der Austellung dieses Ausweises erzeugt wird.";

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
    let message = match (credential_type, language) {
        (VerifiedEmployee(_), English) => employment_consent_msg_eng(),
        (VerifiedEmployee(_), German) => employment_consent_msg_de(),
        (UniversityDegreeCredential(_), English) => degree_consent_msg_eng(),
        (UniversityDegreeCredential(_), German) => degree_consent_msg_de(),
    };
    Icrc21ConsentMessageResponse::Ok(Icrc21ConsentInfo {
        consent_message: message,
        language: format!("{}", language),
    })
}

fn employment_consent_msg_eng() -> String {
    format_message(
        EMPLOYMENT_VC_DESCRIPTION_EN,
        VALIDITY_INFO_EN,
        ANONYMITY_DISCLAIMER_EN,
    )
}

fn employment_consent_msg_de() -> String {
    format_message(
        EMPLOYMENT_VC_DESCRIPTION_DE,
        VALIDITY_INFO_DE,
        ANONYMITY_DISCLAIMER_DE,
    )
}

fn degree_consent_msg_eng() -> String {
    format_message(
        DEGREE_VC_DESCRIPTION_EN,
        VALIDITY_INFO_EN,
        ANONYMITY_DISCLAIMER_EN,
    )
}

fn degree_consent_msg_de() -> String {
    format_message(
        DEGREE_VC_DESCRIPTION_DE,
        VALIDITY_INFO_DE,
        ANONYMITY_DISCLAIMER_DE,
    )
}

fn format_message(description: &str, validity_info: &str, anonymity_disclaimer: &str) -> String {
    format!(
        "{} {}\n\n{}",
        description, validity_info, anonymity_disclaimer
    )
}
