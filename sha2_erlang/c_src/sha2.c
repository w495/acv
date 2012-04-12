// -------------------------------------------------------------------
//
// Copyright (c) 2010 Andrew Tunnell-Jones. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#include "erl_nif.h"
#include <openssl/sha.h>
#include <openssl/hmac.h>

static ERL_NIF_TERM sha224(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary data;
  ERL_NIF_TERM ret;  
  if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
    return enif_make_badarg(env);
  }
  SHA224((unsigned char *) data.data, data.size,
	 enif_make_new_binary(env, SHA224_DIGEST_LENGTH, &ret));
  return ret;
}

static ERL_NIF_TERM sha256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary data;
  ERL_NIF_TERM ret;  
  if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
    return enif_make_badarg(env);
  }
  SHA256((unsigned char *) data.data, data.size,
	 enif_make_new_binary(env, SHA256_DIGEST_LENGTH, &ret));
  return ret;
}

static ERL_NIF_TERM sha384(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
    return enif_make_badarg(env);
  }
  SHA384((unsigned char *) data.data, data.size,
	 enif_make_new_binary(env, SHA384_DIGEST_LENGTH, &ret));
  return ret;
}

static ERL_NIF_TERM sha512(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &data)) {
    return enif_make_badarg(env);
  }
  SHA512((unsigned char *) data.data, data.size,
	 enif_make_new_binary(env, SHA512_DIGEST_LENGTH, &ret));
  return ret;
}

static ERL_NIF_TERM hsha224(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key, data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
      || !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
    return enif_make_badarg(env);
  }
  HMAC(EVP_sha224(), (unsigned char *) key.data, key.size,
       (unsigned char *) data.data, data.size,
       enif_make_new_binary(env, SHA224_DIGEST_LENGTH, &ret), NULL);
  return ret;
}

static ERL_NIF_TERM hsha256(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key, data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
      || !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
    return enif_make_badarg(env);
  }
  HMAC(EVP_sha256(), (unsigned char *) key.data, key.size,
       (unsigned char *) data.data, data.size,
       enif_make_new_binary(env, SHA256_DIGEST_LENGTH, &ret), NULL);
  return ret;
}

static ERL_NIF_TERM hsha384(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key, data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
      || !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
    return enif_make_badarg(env);
  }
  HMAC(EVP_sha384(), (unsigned char *) key.data, key.size,
       (unsigned char *) data.data, data.size,
       enif_make_new_binary(env, SHA384_DIGEST_LENGTH, &ret), NULL);
  return ret;
}

static ERL_NIF_TERM hsha512(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key, data;
  ERL_NIF_TERM ret;
  if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
      || !enif_inspect_iolist_as_binary(env, argv[1], &data)) {
    return enif_make_badarg(env);
  }
  HMAC(EVP_sha512(), (unsigned char *) key.data, key.size,
       (unsigned char *) data.data, data.size,
       enif_make_new_binary(env, SHA512_DIGEST_LENGTH, &ret), NULL);
  return ret;
}

static ErlNifFunc nif_funcs[] =
  {
    {"sha224", 1, sha224},
    {"sha256", 1, sha256},
    {"sha384", 1, sha384},
    {"sha512", 1, sha512},
    {"hmac_sha224", 2, hsha224},
    {"hmac_sha256", 2, hsha256},
    {"hmac_sha384", 2, hsha384},
    {"hmac_sha512", 2, hsha512}
  };

ERL_NIF_INIT(sha2, nif_funcs, NULL,NULL, NULL, NULL);
