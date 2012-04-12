%% -------------------------------------------------------------------
%% Copyright (c) 2010 Andrew Tunnell-Jones. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(sha2).
-export([on_load/0]).
-export([sha224/1, sha256/1, sha384/1, sha512/1]).
-export([hmac_sha224/2, hmac_sha256/2, hmac_sha384/2, hmac_sha512/2]).


-ifndef(TEST).
-on_load(on_load/0).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @private
%% @doc Load the shared object.
on_load() -> 
    So = case code:priv_dir(sha2) of
	     {error, bad_name} ->
		 ModPath = filename:dirname(code:which(?MODULE)),
		 filename:join(ModPath, "../priv/" ?MODULE_STRING);
	     Dir ->
		 filename:join(Dir, ?MODULE_STRING)
	 end,
    case erlang:load_nif(So, 0) of
	{error, {load_failed, Reason}} ->
	    case erlang:system_info(otp_release) of
		"R14" ++ _ ->
		    Format = ?MODULE_STRING " load NIF failed:~n~p~n",
		    error_logger:warning_msg(Format, [Reason]);
		_ -> ok
	    end;
	ok -> ok
    end.

%% @doc sha224(Data :: iodata()) -> Digest :: binary().
-spec sha224(iodata()) -> binary().
sha224(Data) -> sha2_erl:digest224(iolist_to_binary(Data)).

%% @doc sha256(Data :: iodata()) -> Digest :: binary().
-spec sha256(iodata()) -> binary().
sha256(Data) -> sha2_erl:digest256(iolist_to_binary(Data)).

%% @doc sha384(Data :: iodata()) -> Digest :: binary().
-spec sha384(iodata()) -> binary().
sha384(Data) -> sha2_erl:digest384(iolist_to_binary(Data)).

%% @doc sha512(Data :: iodata()) -> Digest :: binary().
-spec sha512(iodata()) -> binary().
sha512(Data) -> sha2_erl:digest512(iolist_to_binary(Data)).

%% @doc hmac_sha224(Key :: iodata(), Data::iodata()) -> MAC :: binary().
-spec hmac_sha224(iodata(), iodata()) -> binary().
hmac_sha224(Key, Data) -> hmac(Key, Data, fun sha2_erl:digest224/1, 64).

%% @doc hmac_sha256(Key :: iodata(), Data::iodata()) -> MAC :: binary().
-spec hmac_sha256(iodata(), iodata()) -> binary().
hmac_sha256(Key, Data) -> hmac(Key, Data, fun sha2_erl:digest256/1, 64).


%% @doc hmac_sha384(Key :: iodata(), Data::iodata()) -> MAC :: binary().
-spec hmac_sha384(iodata(), iodata()) -> binary().
hmac_sha384(Key, Data) -> hmac(Key, Data, fun sha2_erl:digest384/1, 128).

%% @doc hmac_sha512(Key :: iodata(), Data :: iodata()) -> MAC :: binary().
-spec hmac_sha512(iodata(), iodata()) -> binary().
hmac_sha512(Key, Data) -> hmac(Key, Data, fun sha2_erl:digest512/1, 128).

hmac(Key, Data, HashFun, Blocksize)
  when is_binary(Key) andalso is_binary(Data) ->
    %% Shorten keys longer than blocksize
    ShortKey = case byte_size(Key) > Blocksize of
		   true -> HashFun(Key);
		   false -> Key
	       end,
    %% Zero-pad keys shorter than blocksize
    FinalKey = case Blocksize - byte_size(ShortKey) of
		   0 -> Key;
		   PadBy ->
		       ShortKeySize = byte_size(ShortKey),
		     <<ShortKey:ShortKeySize/binary, 0:PadBy/unit:8>>
	       end,
    %% Generate outer and inner pads
    OPad = << <<(16#5C bxor Int)>> || <<Int>> <= FinalKey >>,
    IPad = << <<(16#36 bxor Int)>> || <<Int>> <= FinalKey >>,
    %% Generate inner hash
    InnerHash = HashFun(<<IPad:Blocksize/binary, Data/binary>>),
    %% Generate final hash
    OuterData = <<OPad:Blocksize/binary, InnerHash/binary>>,
    HashFun(OuterData);
hmac(Key, Data, HashFun, BlockSize) ->
    hmac(iolist_to_binary(Key), iolist_to_binary(Data), HashFun, BlockSize).

-ifdef(TEST).

hash_tests() ->
    Data = <<"superperfundo on the early eve of your day">>,
    Cases = [ {sha224, <<6,64,158,208,242,230,202,188,205,202,58,156,34,46,169,157,149,108,77,79,161,150,247,5,143,85,37,143>>},
	      {sha256, <<89,138,155,217,66,158,0,178,212,1,25,158,60,40,200,94,33,128,198,26,134,196,185,151,178,109,55,8,140,210,177,149>>},
	      {sha384, <<118,170,89,73,228,216,207,208,135,64,92,6,155,85,184,156,90,101,19,105,162,66,131,0,68,8,36,26,18,193,46,114,87,110,191,209,170,71,37,120,60,9,164,245,35,21,128,31>>},
	      {sha512, <<149,7,151,81,128,107,68,230,223,57,252,38,148,88,230,7,49,166,228,211,57,103,131,10,99,192,66,32,54,151,173,90,87,3,217,247,195,217,147,103,14,99,11,119,65,125,52,164,139,49,149,158,253,211,244,119,135,160,182,160,238,192,216,96>>}
	    ],
    [ ?_assertEqual(Digest, ?MODULE:Fun(Data)) || {Fun, Digest} <- Cases ].    

hmac_tests() ->
    Key = <<"superperfundo">>,
    Msg = <<"on the early eve of your day">>,
    Cases = [ {hmac_sha224, <<126,122,54,233,134,63,197,75,233,197,104,199,54,139,220,219,214,156,118,208,82,33,7,26,9,71,81,101>>},
	      {hmac_sha256, <<237,115,145,82,10,254,219,14,13,221,173,43,244,92,189,114,166,95,154,62,147,78,170,49,172,64,32,171,23,112,31,145>>},
	      {hmac_sha384, <<139,98,116,191,181,228,111,229,103,125,126,1,199,6,67,188,3,95,170,118,142,220,108,161,176,48,79,7,71,52,74,78,197,234,143,216,213,21,7,50,84,214,89,208,159,40,157,236>>},
	      {hmac_sha512, <<235,182,212,156,168,123,177,223,70,240,251,139,62,149,151,130,139,237,87,69,190,198,136,124,226,165,126,190,117,3,87,178,91,79,131,28,114,146,9,255,67,26,91,34,45,200,253,115,230,79,85,7,50,129,241,39,139,156,148,136,244,232,72,157>>}
	    ],
    [ ?_assertEqual(MAC, ?MODULE:Fun(Key, Msg)) || {Fun, MAC} <- Cases ].

full_test_() ->
    Tests = hash_tests() ++ hmac_tests(),
    [Tests, {setup, fun on_load/0, Tests}].

-endif.
