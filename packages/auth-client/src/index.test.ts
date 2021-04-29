import { AuthClient } from './index';

describe('Auth Client', () => {
  let events = {};
  let idpWindow;

  beforeEach(() => {
    // Mock window.open since we can't open windows here.
    global.open = jest.fn(() => {
      idpWindow = {
        postMessage: jest.fn(),
        close: jest.fn()
      };
      return idpWindow;
    });

		// Empty our events before each test case
		events = [];
		global.addEventListener = jest.fn((event, callback) => {
      events[event] = callback;
    });
	});

  it('should initialize with an AnonymousIdentity', async () => {
    const test = await AuthClient.create();
    expect(await test.isAuthenticated()).toBe(false);
    expect(test.getIdentity().getPrincipal().isAnonymous()).toBe(true);
  });

  it('should open a window with the IDP url', async () => {
    const client = await AuthClient.create();
    // Try without #authorize hash.
    await client.login({ identityProvider: "http://localhost"}, jest.fn(), jest.fn());
    expect(global.open).toBeCalledWith("http://localhost/#authorize", "idpWindow");

    // Try with #authorize hash.
    global.open = jest.fn();
    await client.login({ identityProvider: "http://localhost#authorize"}, jest.fn(), jest.fn());
    expect(global.open).toBeCalledWith("http://localhost/#authorize", "idpWindow");

    // Default url
    global.open = jest.fn();
    await client.login({}, jest.fn(), jest.fn());
    expect(global.open).toBeCalledWith("https://identity.ic0.app/#authorize", "idpWindow");
  });

  it('should ignore authorize-ready events with bad origin', async () => {
    const client = await AuthClient.create();
    await client.login({ identityProvider: "http://localhost#authorize"}, jest.fn(), jest.fn());

    // Send an authorize-ready message with a bad origin. It should _not_ result
    // in a message sent back to the IDP.
    events["message"]({
      origin: "bad origin",
      data: {
        kind: "authorize-ready"
      }
    });

    // No response to the IDP canister.
    expect(idpWindow.postMessage).toBeCalledTimes(0);
  });

  it('should handle authorize-ready events with correct origin', async () => {
    const client = await AuthClient.create();
    await client.login({ identityProvider: "http://localhost#authorize"}, jest.fn(), jest.fn());

    // Send an authorize-ready message with a bad origin. It should _not_ result
    // in a message sent back to the IDP.
    events["message"]({
      origin: global.origin,
      data: {
        kind: "authorize-ready"
      }
    });

    // A response is sent to the IDP.
    expect(idpWindow.postMessage).toBeCalled();
  });

  it('should log users out', async () => {
    const test = await AuthClient.create();
    await test.logout();
    expect(await test.isAuthenticated()).toBe(false);
    expect(test.getIdentity().getPrincipal().isAnonymous()).toBe(true);
  });
});
