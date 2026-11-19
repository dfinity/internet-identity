/** The least lifetime credentials minted before the notification check may have left
 *  when the sign-in hands them over. The allow screen the check can open has no time
 *  limit, so what was minted before it may be about to expire, or expired, once it is
 *  answered; less than this left is minted again, so the app is not handed credentials
 *  that stop working before it has used them. */
export const MIN_REMAINING_LIFETIME_MS = 60_000;
