#sha2_erlang

Erlang NIF wrappers for OpenSSL's SHA224, SHA256, SHA384 and SHA512 digest and HMAC functions. On pre-R14 systems or where the NIF is otherwise unavailable functions fallback to standard Erlang implementations. [Steve Vinoski](http://steve.vinoski.net/)'s BSD licensed [erlsha2 implementation](https://github.com/vinoski/erlsha2) is bundled (as sha2_erl.erl) to faciliate this. Steve's implementation also includes dependancy-free NIFs so if you do not need HMAC functionality you should use it instead of this implementation.

Prerequisites:

- Erlang/OTP R14 (for NIF implementation)
- OpenSSL
