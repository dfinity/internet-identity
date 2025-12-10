This crate defines the new Internet Identity frontend (currently not used in production).

The idea is to factor out the frontend part of the Internet Identity canister, so that the remaining
backend has a simpler Http API and the remaining frontend has a simpler Candid API. This will
make the development process more manageable and better separate concerns.
