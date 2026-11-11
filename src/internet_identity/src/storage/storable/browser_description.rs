use internet_identity_interface::internet_identity::types::{
    BrowserBrand, BrowserDescription, FormFactor, OperatingSystem,
};
use minicbor::{Decode, Encode};

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(map)]
pub struct StorableBrowserDescription {
    #[n(0)]
    pub brand: StorableBrowserBrand,
    #[n(1)]
    pub os: StorableOperatingSystem,
    #[n(2)]
    pub form_factor: StorableFormFactor,
    #[n(3)]
    pub model: Option<String>,
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableBrowserBrand {
    #[n(0)]
    Chrome,
    #[n(1)]
    Safari,
    #[n(2)]
    Firefox,
    #[n(3)]
    Edge,
    #[n(4)]
    Opera,
    #[n(5)]
    SamsungInternet,
    #[n(6)]
    Other(#[n(0)] String),
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StorableOperatingSystem {
    #[n(0)]
    Macos,
    #[n(1)]
    Ios,
    #[n(2)]
    Ipados,
    #[n(3)]
    Windows,
    #[n(4)]
    Android,
    #[n(5)]
    ChromeOs,
    #[n(6)]
    Linux,
    #[n(7)]
    Other(#[n(0)] String),
}

#[derive(Encode, Decode, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
#[cbor(index_only)]
pub enum StorableFormFactor {
    #[n(0)]
    Desktop,
    #[n(1)]
    Mobile,
    #[n(2)]
    Tablet,
    #[n(3)]
    Unknown,
}

impl From<BrowserBrand> for StorableBrowserBrand {
    fn from(value: BrowserBrand) -> Self {
        match value {
            BrowserBrand::Chrome => Self::Chrome,
            BrowserBrand::Safari => Self::Safari,
            BrowserBrand::Firefox => Self::Firefox,
            BrowserBrand::Edge => Self::Edge,
            BrowserBrand::Opera => Self::Opera,
            BrowserBrand::SamsungInternet => Self::SamsungInternet,
            BrowserBrand::Other(token) => Self::Other(token),
        }
    }
}

impl From<StorableBrowserBrand> for BrowserBrand {
    fn from(value: StorableBrowserBrand) -> Self {
        match value {
            StorableBrowserBrand::Chrome => Self::Chrome,
            StorableBrowserBrand::Safari => Self::Safari,
            StorableBrowserBrand::Firefox => Self::Firefox,
            StorableBrowserBrand::Edge => Self::Edge,
            StorableBrowserBrand::Opera => Self::Opera,
            StorableBrowserBrand::SamsungInternet => Self::SamsungInternet,
            StorableBrowserBrand::Other(token) => Self::Other(token),
        }
    }
}

impl From<OperatingSystem> for StorableOperatingSystem {
    fn from(value: OperatingSystem) -> Self {
        match value {
            OperatingSystem::Macos => Self::Macos,
            OperatingSystem::Ios => Self::Ios,
            OperatingSystem::Ipados => Self::Ipados,
            OperatingSystem::Windows => Self::Windows,
            OperatingSystem::Android => Self::Android,
            OperatingSystem::ChromeOs => Self::ChromeOs,
            OperatingSystem::Linux => Self::Linux,
            OperatingSystem::Other(token) => Self::Other(token),
        }
    }
}

impl From<StorableOperatingSystem> for OperatingSystem {
    fn from(value: StorableOperatingSystem) -> Self {
        match value {
            StorableOperatingSystem::Macos => Self::Macos,
            StorableOperatingSystem::Ios => Self::Ios,
            StorableOperatingSystem::Ipados => Self::Ipados,
            StorableOperatingSystem::Windows => Self::Windows,
            StorableOperatingSystem::Android => Self::Android,
            StorableOperatingSystem::ChromeOs => Self::ChromeOs,
            StorableOperatingSystem::Linux => Self::Linux,
            StorableOperatingSystem::Other(token) => Self::Other(token),
        }
    }
}

impl From<FormFactor> for StorableFormFactor {
    fn from(value: FormFactor) -> Self {
        match value {
            FormFactor::Desktop => Self::Desktop,
            FormFactor::Mobile => Self::Mobile,
            FormFactor::Tablet => Self::Tablet,
            FormFactor::Unknown => Self::Unknown,
        }
    }
}

impl From<StorableFormFactor> for FormFactor {
    fn from(value: StorableFormFactor) -> Self {
        match value {
            StorableFormFactor::Desktop => Self::Desktop,
            StorableFormFactor::Mobile => Self::Mobile,
            StorableFormFactor::Tablet => Self::Tablet,
            StorableFormFactor::Unknown => Self::Unknown,
        }
    }
}

impl From<BrowserDescription> for StorableBrowserDescription {
    fn from(value: BrowserDescription) -> Self {
        StorableBrowserDescription {
            brand: value.brand.into(),
            os: value.os.into(),
            form_factor: value.form_factor.into(),
            model: value.model,
        }
    }
}

impl From<StorableBrowserDescription> for BrowserDescription {
    fn from(value: StorableBrowserDescription) -> Self {
        BrowserDescription {
            brand: value.brand.into(),
            os: value.os.into(),
            form_factor: value.form_factor.into(),
            model: value.model,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn round_trip(description: StorableBrowserDescription) {
        let mut buffer = Vec::new();
        minicbor::encode(&description, &mut buffer).unwrap();
        assert_eq!(
            minicbor::decode::<StorableBrowserDescription>(&buffer).unwrap(),
            description
        );
    }

    #[test]
    fn a_description_of_named_tokens_round_trips() {
        round_trip(StorableBrowserDescription {
            brand: StorableBrowserBrand::Safari,
            os: StorableOperatingSystem::Ipados,
            form_factor: StorableFormFactor::Tablet,
            model: None,
        });
    }

    /// The variants that carry a token are the ones a decoder could get wrong, because
    /// the payload has to be read back with the variant that named it.
    #[test]
    fn a_description_of_unrecognised_tokens_round_trips() {
        round_trip(StorableBrowserDescription {
            brand: StorableBrowserBrand::Other("YaBrowser".to_string()),
            os: StorableOperatingSystem::Other("HarmonyOS".to_string()),
            form_factor: StorableFormFactor::Unknown,
            model: Some("Pixel 9".to_string()),
        });
    }

    /// Every token the interface can hold has a storable counterpart and comes back as
    /// itself. Written as a sweep so adding a variant to one side without the other
    /// fails here rather than silently mapping to something else.
    #[test]
    fn every_token_survives_the_trip_through_storage() {
        let brands = [
            BrowserBrand::Chrome,
            BrowserBrand::Safari,
            BrowserBrand::Firefox,
            BrowserBrand::Edge,
            BrowserBrand::Opera,
            BrowserBrand::SamsungInternet,
            BrowserBrand::Other("Arc".to_string()),
        ];
        for brand in brands {
            assert_eq!(
                BrowserBrand::from(StorableBrowserBrand::from(brand.clone())),
                brand
            );
        }

        let systems = [
            OperatingSystem::Macos,
            OperatingSystem::Ios,
            OperatingSystem::Ipados,
            OperatingSystem::Windows,
            OperatingSystem::Android,
            OperatingSystem::ChromeOs,
            OperatingSystem::Linux,
            OperatingSystem::Other("HarmonyOS".to_string()),
        ];
        for os in systems {
            assert_eq!(
                OperatingSystem::from(StorableOperatingSystem::from(os.clone())),
                os
            );
        }

        for form_factor in [
            FormFactor::Desktop,
            FormFactor::Mobile,
            FormFactor::Tablet,
            FormFactor::Unknown,
        ] {
            assert_eq!(
                FormFactor::from(StorableFormFactor::from(form_factor.clone())),
                form_factor
            );
        }
    }
}
