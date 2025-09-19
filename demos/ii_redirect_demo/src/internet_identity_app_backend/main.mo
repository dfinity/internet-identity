import Principal "mo:base/Principal";
import Debug "mo:base/Debug";

persistent actor Whoami {
  public query (message) func whoami() : async Principal {
    Debug.print("Received whoami query");
    let caller = message.caller;
    Debug.print("Caller Principal: " # Principal.toText(caller));
    caller;
  };
};
