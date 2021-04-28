import { AnonymousIdentity, blobFromUint8Array, derBlobFromBlob, Identity, Principal, SignIdentity } from '@dfinity/agent';
import {
  createAuthenticationRequestUrl,
  createDelegationChainFromAccessToken,
  getAccessTokenFromWindow,
  isDelegationValid,
} from '@dfinity/authentication';
import { Delegation, DelegationChain, DelegationIdentity, Ed25519KeyIdentity } from '@dfinity/identity';

const KEY_LOCALSTORAGE_KEY = 'identity';
const KEY_LOCALSTORAGE_DELEGATION = 'delegation';
const IDENTITY_PROVIDER_DEFAULT = "https://identity.ic0.app";
const IDENTITY_PROVIDER_ENDPOINT = "#authorize";

/**
 * List of options for creating an {@link AuthClient}.
 */
export interface AuthClientOptions {
  /**
   * Identity provider. By default, use the identity service.
   */
  identityProvider?: string | URL;

  /**
   * An identity to use as the base
   */
  identity?: SignIdentity;
  storage?: AuthClientStorage;
}

export interface AuthClientStorage {
  get(key: string): Promise<string | null>;

  set(key: string, value: string): Promise<void>;

  remove(key: string): Promise<void>;
}

async function _deleteStorage(storage: AuthClientStorage) {
  await storage.remove(KEY_LOCALSTORAGE_KEY);
  await storage.remove(KEY_LOCALSTORAGE_DELEGATION);
}

export class LocalStorage implements AuthClientStorage {
  constructor(public readonly prefix = 'ic-', private readonly _localStorage?: Storage) {}

  public get(key: string): Promise<string | null> {
    return Promise.resolve(this._getLocalStorage().getItem(this.prefix + key));
  }

  public set(key: string, value: string): Promise<void> {
    this._getLocalStorage().setItem(this.prefix + key, value);
    return Promise.resolve();
  }

  public remove(key: string): Promise<void> {
    this._getLocalStorage().removeItem(this.prefix + key);
    return Promise.resolve();
  }

  private _getLocalStorage() {
    if (this._localStorage) {
      return this._localStorage;
    }

    const ls =
      typeof window === 'undefined'
        ? typeof global === 'undefined'
          ? typeof self === 'undefined'
            ? undefined
            : self.localStorage
          : global.localStorage
        : window.localStorage;

    if (!ls) {
      throw new Error('Could not find local storage.');
    }

    return ls;
  }
}

export class AuthClient {
  public static async create(options: AuthClientOptions = {}): Promise<AuthClient> {
    const storage = options.storage ?? new LocalStorage('ic-');

    let key : (null | SignIdentity) = null ;
    if (options.identity) {
      key = options.identity;
    } else {
      const maybeIdentityStorage = await storage.get(KEY_LOCALSTORAGE_KEY);
      if (maybeIdentityStorage) {
        try {
          key = Ed25519KeyIdentity.fromJSON(maybeIdentityStorage);
        } catch (e) {
          // Ignore this, this means that the localStorage value isn't a valid Ed25519KeyIdentity
          // serialization.
        }
      }
    }

    let identity = new AnonymousIdentity();
    let chain : (null | DelegationChain) = null;

    if (key) {
      try {
        const chainStorage = localStorage.getItem(KEY_LOCALSTORAGE_DELEGATION);
        if (chainStorage) {
          chain = DelegationChain.fromJSON(chainStorage);

          // Verify that the delegation isn't expired.
          if (!isDelegationValid(chain)) {
            await _deleteStorage(storage);
            key = null;
          } else {
            identity = DelegationIdentity.fromDelegation(key, chain);
          }
        }
      } catch (e) {
        // If there was a problem loading the chain, delete the key.
        await _deleteStorage(storage);
        key = null;
      }
    }

    return new this(identity, key, chain, storage);
  }

  protected constructor(
    private _identity: Identity,
    private _key: SignIdentity | null,
    private _chain: DelegationChain | null,
    private _storage: AuthClientStorage,
    // A handle on the IdP window.
    private _idpWindow?: Window,
    // A controller used to remove the event listener in the login flow.
    private _abortController?: AbortController,
    // @ts-ignore (typescript doesn't know the Timeout type)
    private _pingInterval?: Timeout,
  ) {}

  public getIdentity(): Identity {
    return this._identity;
  }

  public async isAuthenticated(): Promise<boolean> {
    return !this.getIdentity().getPrincipal().isAnonymous() && this._chain !== null;
  }

  public async login(
    options: { identityProvider?: string; maxTimeToLive?: BigInt } = {},
    onSuccess,
    onError
  ): Promise<void> {
    let key = this._key;
    if (!key) {
      // Create a new key (whether or not one was in storage).
      key = Ed25519KeyIdentity.generate();
      this._key = key;
      await this._storage.set(KEY_LOCALSTORAGE_KEY, JSON.stringify(key));
    }

    // Create the URL of the IDP. (e.g. https://XXXX/#authorize)
    let identityProviderUrl = new URL(options.identityProvider || IDENTITY_PROVIDER_DEFAULT);
    // Set the correct hash if it isn't already set.
    identityProviderUrl.hash = IDENTITY_PROVIDER_ENDPOINT;

    // If `login` has been called previously, then close/remove any previous windows
    // and event listeners.
    this._idpWindow?.close();
    this._abortController?.abort();

    // Add an event listener to handle responses.
    // The event listener is associated with a controller that signals the listener
    // to remove itself as soon as authentication is complete.
    this._abortController = new AbortController();
    // @ts-ignore (typescript doesn't understand adding an event listener with a signal).
    window.addEventListener("message", async (event) => {
      if (event.origin !== identityProviderUrl.origin) {
        console.log(`Received an event from origin ${event.origin} but expected ${identityProviderUrl.origin}`);
        return;
      }

      const message = event.data;
      console.log(`Received ${message.kind} message.`)

      switch (message.kind) {
        case "authorize-ready":
          // IDP is ready. Pinging is no longer needed.
          clearInterval(this._pingInterval);

          // Send a message to request authorization.
          this._idpWindow?.postMessage({
            kind: "authorize-client",
            sessionPublicKey: this._key?.getPublicKey().toDer(),
            maxTimeToLive: options.maxTimeToLive
          }, identityProviderUrl.origin);
          break;
        case "authorize-client-success":
          // Create delegation chain and store it.
          const delegationChain = DelegationChain.fromDelegations(
            [
              {
                delegation: new Delegation(
                  event.data.delegations[0].delegation.pubkey,
                  event.data.delegations[0].delegation.expiration
                ),
                signature: event.data.delegations[0].signature
              }
            ],
            derBlobFromBlob(blobFromUint8Array(Uint8Array.from(event.data.userPublicKey)))
          );

          const key = this._key;
          if (!key) {
            console.log("No key found");
            return;
          }

          this._chain = delegationChain;
          await this._storage.set(KEY_LOCALSTORAGE_DELEGATION, JSON.stringify(this._chain.toJSON()));
          this._identity = DelegationIdentity.fromDelegation(key, this._chain);

          this._idpWindow?.close();
          onSuccess();
          this._abortController?.abort(); // Send the abort signal to remove event listener.
          break;
        case "authorize-client-failure":
          this._idpWindow?.close();
          onError(message.text);
          this._abortController?.abort(); // Send the abort signal to remove event listener.
          break;
        default:
          console.log(`Unknown message of kind ${message.kind}`)
      }
    }, { signal: this._abortController.signal });

    // Open a new window with the IDP provider.
    const w = window.open(identityProviderUrl.toString(), "idpWindow");
    if (w) {
      this._idpWindow = w;
    }

    // Periodically send pings to the IDP. When the IDP receives the ping
    // it can communicate to us securely that it is fully loaded, the user
    // has logged in, and it is ready to receive authentication requests.
    if (this._pingInterval) {
      clearInterval(this._pingInterval);
    }

    this._pingInterval = setInterval( () => {
      if (this._idpWindow && !this._idpWindow.closed) {
        console.log("Sending authorize-ping.")
        this._idpWindow.postMessage({ kind: "authorize-ping" }, identityProviderUrl.origin)
      } else {
        clearInterval(this._pingInterval)
      }
    }, 500);
  }

  public async logout(options: { returnTo?: string } = {}): Promise<void> {
    _deleteStorage(this._storage);

    // Reset this auth client to a non-authenticated state.
    this._identity = new AnonymousIdentity();
    this._key = null;
    this._chain = null;

    if (options.returnTo) {
      try {
        window.history.pushState({}, '', options.returnTo);
      } catch (e) {
        window.location.href = options.returnTo;
      }
    }
  }
}
