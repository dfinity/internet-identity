import { MetadataMapV2, UserNumber } from "$generated/internet_identity_types";
import { Principal } from "@dfinity/principal";

export type JWT = string;
export type Salt = Uint8Array;

export interface OpenIDCredential {
  iss: string;
  sub: string;
  aud: string;
  principal: Principal;
  last_usage_timestamp: bigint;
  metadata: MetadataMapV2;
}

// Mocked implementation of new or existing actor methods with only additional data
export class MockOpenID {
  #credentials: OpenIDCredential[] = [];

  add_jwt(
    _userNumber: UserNumber,
    jwt: JWT,
    _salt: Salt
  ): Promise<{ Ok: null } | { Err: string }> {
    const [_header, body, _signature] = jwt.split(".");
    const { iss, sub, aud, email, name, picture } = JSON.parse(atob(body));
    if (
      this.#credentials.find(
        (credential) => credential.iss === iss && credential.sub === sub
      )
    ) {
      return Promise.resolve({ Err: "This account has already been linked" });
    }
    this.#credentials.push({
      iss,
      sub,
      aud,
      principal: Principal.anonymous(),
      last_usage_timestamp: BigInt(Date.now()) * BigInt(1000000),
      metadata: [
        ["email", { String: email }],
        ["name", { String: name }],
        ["picture", { String: picture }],
      ],
    });
    return Promise.resolve({ Ok: null });
  }

  remove_jwt(
    _userNumber: UserNumber,
    iss: string,
    sub: string
  ): Promise<{ Ok: null } | { Err: string }> {
    const index = this.#credentials.findIndex(
      (credential) => credential.iss === iss && credential.sub === sub
    );
    this.#credentials.splice(index, 1);
    return Promise.resolve({ Ok: null });
  }

  get_anchor_info(_userNumber: UserNumber): Promise<{
    credentials: OpenIDCredential[];
  }> {
    return Promise.resolve({ credentials: this.#credentials });
  }
}
