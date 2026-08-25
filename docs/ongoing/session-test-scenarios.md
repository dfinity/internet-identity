# Session test scenarios

**Designs:** [shared-revocable-sessions.md](shared-revocable-sessions.md) ties together [tracked-default-accounts.md](tracked-default-accounts.md), [revocable-app-sessions.md](revocable-app-sessions.md), [silent-reauth-redirect.md](silent-reauth-redirect.md) and [client-app-sessions.md](client-app-sessions.md). This document assumes them and does not repeat what they explain.

## Summary

What the four designs together promise is that a user's access to an app can be ended, that it survives across tabs and sibling subdomains without a second sign-in, and that none of it requires the user to notice a five-minute delegation being replaced underneath them. Every one of those promises is about something a user can reach and see, which is where the canister's own tests stop.

This page is the list of those. Each scenario states the state it starts from, what is done, and what must then be true. They are written to be run, by a person or by a machine, and to be cited: an audit walks the ids and returns a verdict per id.

## What belongs here

A scenario belongs here when a user can reach it and see the result. Reaching it means doing the things a user does:

1. **Using an app.**  
   Visiting it, signing in, doing something that makes it talk to its canister, and leaving it open.
2. **Managing windows.**  
   Opening a second tab, closing one, reloading, closing the browser and coming back, visiting a sibling subdomain.
3. **Waiting.**  
   Long enough for a delegation to age, or for a session to expire.
4. **Using the identity's own settings.**  
   Reading the list of apps and browsers, and ending a sign-in from it.
5. **Starting over.**  
   Clearing the site's data, and signing in from a second browser.

Seeing the result means one of: the app works or it does not, it shows the same account or a different one, it asks for a sign-in or it does not, settings says something different, or a screen the design promises never to render stays absent.

That rules out a great deal the designs specify, and deliberately. Anything reachable only by an app calling the library differently, by reading what is stored, or by making a call fail on purpose, is tested where it lives. The [omissions](#omissions) name those categories.

Where a scenario needs a delegation to age, the point is the boundary and not the wait. Reaching the state some other way is fine, so long as what is asserted is the behaviour at the boundary.

## Signing in

1. **FIRST-1. A first sign-in at an app the identity has never used.**  
   Start with an identity that has never signed in at the app.  
   Sign in and use it.  
   It must work, and the app must now appear in the identity's list with a last-used time.  
   Covers NEW-1, MINT-12, LIMIT-1.
2. **FIRST-2. The account stays the same for as long as the app is open.**  
   Sign in, note the account the app shows, then keep using it for longer than one delegation lasts.  
   It must show that same account throughout, with no moment where it appears to be someone else or nobody.  
   Covers MINT-16, MINT-17, MINT-2.
3. **FIRST-3. Signing in twice from one browser leaves one sign-in.**  
   Sign in, then sign in again from the same browser to the same account.  
   Settings must show one sign-in for that pairing rather than two, and the app must still work.  
   Covers NEW-4, REC-9.

## Staying signed in

1. **HOLD-1. The app keeps working for longer than a delegation lasts.**  
   Sign in, then keep using the app past the point where a single delegation would have expired.  
   It must keep working, and must ask for nothing.  
   Covers USE-4, MINT-3, MINT-11.
2. **HOLD-2. An app left open and untouched asks for nothing.**  
   Sign in, leave the tab open and idle for a long time, then use it.  
   Nothing must have been asked of the user while it sat there, and it must work when used.  
   Covers MINT-5, MINT-14.
3. **HOLD-3. Coming back to a tab left for a while does not stall it.**  
   Sign in, switch away long enough for the delegation to be due for replacement, come back, and use the app.  
   The first thing done after returning must not visibly wait.  
   Covers MINT-7, MINT-13, MINT-6.
4. **HOLD-4. Nothing appears while the app is in use.**  
   Keep using the app across the point where its delegation has to be replaced.  
   No popup, no navigation and no II screen may appear.  
   Covers USE-6, MINT-4.
5. **HOLD-5. Settings shows the app being used.**  
   Note what settings reports for the app and for the browser, use the app for a while, and look again.  
   Both last-used times must have advanced.  
   Covers USE-5, DEV-17.
6. **HOLD-6. A reload asks for nothing.**  
   Sign in, reload the page, and use the app.  
   It must come back as the same account, having asked for nothing.  
   Covers ACQ-5, MINT-13.

## More than one tab, and sibling subdomains

1. **SHARE-1. A second tab is already signed in.**  
   Sign in on one tab, then open the app in a second tab.  
   The second must arrive as the same account, with no sign-in screen.  
   Covers TAB-1, ACQ-5.
2. **SHARE-2. Signing out in one tab is noticed in the other.**  
   With two tabs open, sign out in one, then use the other.  
   The second must stop acting as that account rather than carrying on, and must offer a sign-in.  
   Covers ERR-1, FAIL-2.
3. **SHARE-3. A sibling subdomain is already signed in.**  
   Sign in on one subdomain of a shared domain, then visit a sibling that has never been signed in to.  
   The sibling must reach a working state as the same account, showing no II screen.  
   Covers SIL-1, SIL-3, SIL-5, HINT-2.
4. **SHARE-4. A sibling asks properly when there is nothing left to resume from.**  
   Sign in on one subdomain, wait for the session to expire, then visit a sibling.  
   It must ask the user to sign in, rather than failing in a way the user cannot act on.  
   Covers HINT-5, FAIL-1, SIL-4.
5. **SHARE-5. Signing out on one subdomain signs the siblings out.**  
   Sign in on one subdomain, use a sibling, then sign out on the first and use the sibling again.  
   The sibling must stop acting as that account and must ask for a sign-in.  
   Covers HINT-3, END-3.
6. **SHARE-6. Closing a tab does not disturb the others.**  
   With several tabs of the app open and in use, close some, then use one of those left.  
   It must keep working without a pause.  
   Covers TAB-9, TAB-3.

## Ending a session

1. **EXIT-1. Signing out of one app leaves the others alone.**  
   Sign in at two apps with one identity, sign out of one, and use both.  
   The one signed out of must ask for a sign-in; the other must be unaffected.  
   Covers END-1, END-3.
2. **EXIT-2. The owner ends a sign-in from settings.**  
   Sign in, end that sign-in from settings, and keep using the app.  
   The app must lose access, and the identity's other apps must keep working.  
   Covers END-2.
3. **EXIT-3. The owner signs a whole browser out.**  
   Sign in at several apps from one browser, sign that browser out from settings, and use each app.  
   All must lose access.  
   Covers END-2, END-6, DEV-15.
4. **EXIT-4. Access ends soon after it is revoked, neither instantly nor much later.**  
   Note the moment a sign-in is ended from settings, then keep using the app.  
   It may keep working briefly, and must have stopped within the life of one delegation.  
   Covers END-5.
5. **EXIT-5. A browser signed out is still the same browser.**  
   Sign a browser out from settings, then sign in again from it and look at the list.  
   It must hold one entry for that browser, not two.  
   Covers DEV-18.
6. **EXIT-6. Signing out leaves nothing behind.**  
   Sign out, then reload the page.  
   The app must come back signed out, and must not briefly show the old account first.  
   Covers END-1, ERR-3.
7. **EXIT-7. An expired session and a revoked one look alike.**  
   Reach the point where the app can no longer act, once by ending the sign-in from settings and once by letting the session expire.  
   The user must be shown the same thing either way: an app asking for a sign-in.  
   Covers END-4, FAIL-1.

## Using many apps and browsers

1. **CAP-1. Using a great many apps never breaks the next sign-in.**  
   Sign in at more apps than the identity keeps rows for, then sign in at one more.  
   It must work.  
   Covers LIMIT-2, LIMIT-3.
2. **CAP-2. An app dropped for disuse is unchanged on return.**  
   Reach the state where an idle app's row has been dropped, then sign in there again.  
   It must show the same account as before, and whatever it stored for that account must still be there.  
   Covers LIMIT-7.
3. **CAP-3. The app just signed into keeps working.**  
   Sign in at the limit, then keep using the app just signed into.  
   It must not be the one dropped.  
   Covers LIMIT-6.
4. **CAP-4. Signing in many times never starts failing.**  
   Sign in far more times than the identity keeps sign-ins for.  
   Every one must succeed.  
   Covers REC-6, REC-7, REC-8.
5. **CAP-5. An app still signed in can be dropped, and asks again.**  
   Reach the row limit with the least recently used app still signed in, sign in somewhere new, then return to that app.  
   It must ask for a sign-in rather than half-working.  
   Covers LIMIT-4, LIMIT-5.
6. **CAP-6. Signing in from many browsers drops the oldest.**  
   Sign in from more browsers than the list holds, then use the app from the browser signed in longest ago.  
   It must ask for a sign-in, and the recent browsers must be the ones listed.  
   Covers DEV-14, DEV-15.

## What the owner sees

1. **SHOW-1. An app appears once it has been used.**  
   Sign in at a new app and open settings.  
   It must be listed, with when it was last used.  
   Covers LIMIT-1, USE-5.
2. **SHOW-2. A browser appears under a name that identifies it.**  
   Sign in from a browser and open settings.  
   It must be listed, named recognisably, with when it was last used.  
   Covers DEV-1, DEV-17, DEV-19.
3. **SHOW-3. A sign-in says which browser made it.**  
   Open settings with a sign-in in place.  
   It must say which browser it came from and when, and that must not change afterwards except the last-used time.  
   Covers REC-1, REC-2.
4. **SHOW-4. Ending a sign-in reaches the app that is running.**  
   With an app open and working, end its sign-in from settings, then keep using the app.  
   It must lose access without the user touching it.  
   Covers END-2, END-5.

## Coming back later

1. **STAY-1. Closing the browser and returning asks for nothing.**  
   Sign in, close the browser entirely, reopen it and return to the app.  
   It must be signed in as the same account.  
   Covers ACQ-5, KEY-1.
2. **STAY-2. Clearing the site's data is a clean start.**  
   Sign in, clear the site's data, and reload.  
   The app must be signed out, and signing in again must work.  
   Covers ERR-3.
3. **STAY-3. An interrupted sign-in can be retried.**  
   Close the window part way through signing in, then sign in again from the same browser.  
   The second attempt must succeed, and settings must show one entry for that browser.  
   Covers DEV-10, DEV-13, DEV-11.
4. **STAY-4. Two identities in one browser stay apart.**  
   Sign in as one identity at an app, sign out, then sign in as another at the same app.  
   The app must show a different account, with nothing of the first still visible to it.  
   Covers DEV-6.

## Omissions

Every scenario cites the requirements it exercises, so the specs can be walked in the other direction. Most requirements have no scenario here, which is what the rule at the top is for: a user cannot reach them. These are the categories, and where each is tested instead:

| Left out                                                                         | Why                                                                            |
| -------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| An app calling the library differently, or asking for what it is not entitled to | Not reachable from an app that behaves; the library's and canister's own tests |
| Reading what is stored, or what passes between subdomains                        | Nothing a user sees; asserted where the storage is written                     |
| Making a call fail on purpose to see what survives                               | Not reachable by using an app; the library's failure tests                     |
| Whether two tabs mint once or twice                                              | A cost rather than a behaviour: what a user sees is that both tabs work        |
| Agent construction and the shape of what an app is handed                        | Visible to a caller's types, not to a user                                     |
| The browser proof's cryptography and its key rotation                            | A signature either verifies or it does not; the canister's tests               |
| Canister-internal ordering, indexes and number allocation                        | Not reachable from a browser at all                                            |

What is left is the set whose whole point is what a user experiences, and those are the ones to keep honest: END-5 on how long access outlives revocation, LIMIT-7 on an evicted account being unchanged, MINT-16 on the account never appearing to change under a working app, HINT-5 on an expired session still leading somewhere, and FAIL-1 on a user being offered a sign-in rather than a dead end.
