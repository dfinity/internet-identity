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

  add_jwt(_userNumber: UserNumber, jwt: JWT, _salt: Salt): Promise<void> {
    const [_header, body, _signature] = jwt.split(".");
    const { iss, sub, aud, email, name, picture } = JSON.parse(atob(body));
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
    return Promise.resolve();
  }

  remove_jwt(_userNumber: UserNumber, iss: string, sub: string): Promise<void> {
    const index = this.#credentials.findIndex(
      (credential) => credential.iss === iss && credential.sub === sub
    );
    this.#credentials.splice(index, 1);
    return Promise.resolve();
  }

  get_anchor_info(_userNumber: UserNumber): Promise<{
    credentials: OpenIDCredential[];
  }> {
    return Promise.resolve({ credentials: this.#credentials });
  }
}
