//! The one room: who is in it and what was said, newest last.
//!
//! Everything here is plain state and the rules over it, so it can be tested
//! without a canister. What a change means for notifications is decided by
//! the caller: [`Room::post`] says who should hear about a message, and the
//! canister sends.

use candid::{CandidType, Deserialize, Principal};

/// Beyond this a join is refused rather than evicting somebody: a tester who
/// hits it leaves, and takes a seat with them.
pub const MAX_MEMBERS: usize = 100;

/// Only the newest are kept; the room is read whole by `room`.
pub const MAX_MESSAGES: usize = 100;

/// Characters, not bytes. A longer message is cut, not refused.
pub const MAX_TEXT_CHARS: usize = 500;

#[derive(Clone, Debug, PartialEq, Eq, CandidType, Deserialize)]
pub struct Member {
    pub who: Principal,
    /// Nanoseconds since the epoch.
    pub joined_at: u64,
}

#[derive(Clone, Debug, PartialEq, Eq, CandidType, Deserialize)]
pub struct Message {
    pub from: Principal,
    /// Nanoseconds since the epoch.
    pub at: u64,
    pub text: String,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, CandidType, Deserialize)]
pub struct Room {
    pub members: Vec<Member>,
    pub messages: Vec<Message>,
    /// The claimed names of the members and of whoever wrote a message
    /// still in the room. Filled in by the canister when the room is read.
    pub names: Vec<(Principal, String)>,
}

/// Why the room would not do what was asked.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Refused {
    RoomFull,
    NotAMember,
}

impl Refused {
    pub fn message(self) -> &'static str {
        match self {
            Refused::RoomFull => "the room is full",
            Refused::NotAMember => "join the room first",
        }
    }
}

/// A message the room took, and who should hear about it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Posted {
    pub message: Message,
    /// Everyone in the room but the author.
    pub recipients: Vec<Principal>,
}

impl Room {
    /// Joins, or does nothing for somebody already in.
    pub fn join(&mut self, who: Principal, now: u64) -> Result<(), Refused> {
        if self.is_member(who) {
            return Ok(());
        }
        if self.members.len() >= MAX_MEMBERS {
            return Err(Refused::RoomFull);
        }
        self.members.push(Member {
            who,
            joined_at: now,
        });
        Ok(())
    }

    /// Nothing happens for somebody who is not in.
    pub fn leave(&mut self, who: Principal) {
        self.members.retain(|member| member.who != who);
    }

    pub fn is_member(&self, who: Principal) -> bool {
        self.members.iter().any(|member| member.who == who)
    }

    /// Takes a message from a member. Long messages are cut to
    /// [`MAX_TEXT_CHARS`], and only the newest [`MAX_MESSAGES`] are kept.
    pub fn post(&mut self, from: Principal, text: &str, now: u64) -> Result<Posted, Refused> {
        if !self.is_member(from) {
            return Err(Refused::NotAMember);
        }

        let message = Message {
            from,
            at: now,
            text: text.chars().take(MAX_TEXT_CHARS).collect(),
        };
        self.messages.push(message.clone());
        if self.messages.len() > MAX_MESSAGES {
            let surplus = self.messages.len() - MAX_MESSAGES;
            self.messages.drain(..surplus);
        }

        Ok(Posted {
            message,
            recipients: self
                .members
                .iter()
                .map(|member| member.who)
                .filter(|member| *member != from)
                .collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn member(n: u64) -> Principal {
        Principal::from_slice(&n.to_be_bytes())
    }

    #[test]
    fn joining_again_changes_nothing() {
        let mut room = Room::default();
        assert_eq!(room.join(member(1), 10), Ok(()));
        assert_eq!(room.join(member(1), 20), Ok(()));

        assert_eq!(room.members.len(), 1);
        assert_eq!(room.members[0].joined_at, 10);
    }

    #[test]
    fn leaving_takes_only_the_caller_out() {
        let mut room = Room::default();
        room.join(member(1), 10).unwrap();
        room.join(member(2), 20).unwrap();

        room.leave(member(1));
        room.leave(member(3));

        assert_eq!(room.members.len(), 1);
        assert!(room.is_member(member(2)));
    }

    #[test]
    fn a_full_room_refuses_the_next_member_until_somebody_leaves() {
        let mut room = Room::default();
        for n in 0..MAX_MEMBERS as u64 {
            assert_eq!(room.join(member(n), n), Ok(()), "the room has room");
        }

        assert_eq!(
            room.join(member(MAX_MEMBERS as u64), 1_000),
            Err(Refused::RoomFull)
        );
        assert_eq!(
            room.join(member(0), 1_000),
            Ok(()),
            "already in, so not refused"
        );

        room.leave(member(0));
        assert_eq!(room.join(member(MAX_MEMBERS as u64), 1_000), Ok(()));
    }

    #[test]
    fn a_message_goes_to_everyone_else_in_the_room() {
        let mut room = Room::default();
        room.join(member(1), 10).unwrap();
        room.join(member(2), 20).unwrap();
        room.join(member(3), 30).unwrap();

        let posted = room.post(member(2), "hello", 40).unwrap();

        assert_eq!(posted.recipients, vec![member(1), member(3)]);
        assert_eq!(
            room.messages,
            vec![Message {
                from: member(2),
                at: 40,
                text: "hello".to_string(),
            }]
        );
    }

    #[test]
    fn only_a_member_can_post() {
        let mut room = Room::default();
        room.join(member(1), 10).unwrap();

        assert_eq!(room.post(member(2), "hello", 20), Err(Refused::NotAMember));
        assert!(room.messages.is_empty());
    }

    #[test]
    fn a_long_message_is_cut_and_old_ones_fall_off() {
        let mut room = Room::default();
        room.join(member(1), 10).unwrap();

        let long = "x".repeat(MAX_TEXT_CHARS + 1);
        let posted = room.post(member(1), &long, 20).unwrap();
        assert_eq!(posted.message.text.chars().count(), MAX_TEXT_CHARS);

        for n in 0..MAX_MESSAGES as u64 {
            room.post(member(1), &n.to_string(), 100 + n).unwrap();
        }
        assert_eq!(room.messages.len(), MAX_MESSAGES);
        assert_eq!(
            room.messages[0].text, "0",
            "the cut message was the oldest, and went"
        );
    }
}
