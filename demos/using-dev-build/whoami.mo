import Principal "mo:base/Principal";

actor Whoami {
  public query (message) func whoami() : async Principal {
    message.caller;
  };
};
