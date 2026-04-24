use crate::state;
use crate::storage::storable::smtp::StorableEmail;
use internet_identity_interface::internet_identity::types::smtp::{
    validate_envelope_only, DkimVerificationStatus, PostboxEmail, SmtpRequest, SmtpResponse,
    ValidatedSmtpRequest,
};
use internet_identity_interface::internet_identity::types::AnchorNumber;

pub fn handle_smtp_request(request: SmtpRequest) -> SmtpResponse {
    let validated: ValidatedSmtpRequest = match request.try_into() {
        Ok(v) => v,
        Err(response) => return response,
    };

    let recipient_key = validated.anchor_number.to_string();

    let email = StorableEmail {
        sender: validated.sender,
        recipient: validated.recipient,
        subject: validated.subject,
        body: validated.body,
        dkim_status: Some(DkimVerificationStatus::Pending),
    };

    // Spawn async DKIM verification (best-effort, fire-and-forget), then
    // dispatch push notifications only if verification succeeded. Unverified
    // emails are still stored and visible in the postbox, they just don't
    // interrupt the user with a device notification.
    // In non-test builds, the headers/body/index are consumed by the spawned task.
    // In test builds, the spawn block is stripped so we suppress unused warnings.
    #[cfg(not(test))]
    {
        let headers = validated.headers;
        let raw_body = validated.raw_body;
        let email_index =
            state::storage_borrow_mut(|storage| storage.store_email(recipient_key.clone(), email));

        ic_cdk::spawn(async move {
            let status = crate::dkim::verify_email_dkim(&headers, &raw_body).await;
            let verified = matches!(&status, DkimVerificationStatus::Verified { .. });
            state::storage_borrow_mut(|storage| {
                storage.update_email_dkim_status(recipient_key.clone(), email_index, status);
            });
            if verified {
                crate::web_push::dispatch_push_for_anchor(&recipient_key).await;
            }
        });
    }

    #[cfg(test)]
    {
        let _ = &validated.headers;
        let _ = &validated.raw_body;
        state::storage_borrow_mut(|storage| {
            storage.store_email(recipient_key, email);
        });
    }

    SmtpResponse::Ok {}
}

pub fn get_postbox(anchor_number: AnchorNumber) -> Vec<PostboxEmail> {
    state::storage_borrow(|storage| {
        storage
            .get_emails(&anchor_number.to_string())
            .into_iter()
            .rev()
            .map(|e| PostboxEmail {
                sender: e.sender,
                recipient: e.recipient,
                subject: e.subject,
                body: e.body,
                dkim_status: e.dkim_status,
            })
            .collect()
    })
}

pub fn handle_smtp_request_validate(request: SmtpRequest) -> SmtpResponse {
    // If a full message is present, run full validation
    if request.message.is_some() {
        return match ValidatedSmtpRequest::try_from(request) {
            Ok(_) => SmtpResponse::Ok {},
            Err(response) => response,
        };
    }

    // Otherwise validate just the envelope (and whatever else is present)
    match validate_envelope_only(&request) {
        Ok(()) => SmtpResponse::Ok {},
        Err(response) => response,
    }
}
