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
    const { iss, sub, aud } = JSON.parse(atob(body));
    this.#credentials.push({
      iss,
      sub,
      aud,
      principal: Principal.anonymous(),
      last_usage_timestamp: BigInt(Date.now()) * BigInt(1000000),
      metadata: [],
    });
    return Promise.resolve({ Ok: null });
  }

  get_anchor_info(_userNumber: UserNumber): Promise<{
    credentials: OpenIDCredential[];
  }> {
    return Promise.resolve({ credentials: this.#credentials });
  }
}
