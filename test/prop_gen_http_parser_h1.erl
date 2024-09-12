-module(prop_gen_http_parser_h1).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

prop_encoded_chunk_always_contains_at_least_two_crlfs() ->
    ?FORALL(Chunk, iodata(), begin
        Encoded = gen_http_parser_h1:encode_chunk(Chunk),
        [Tail | Rest] = lists:reverse(Encoded),
        ?assertEqual("\r\n", Tail),
        ?assertEqual(true, lists:member("\r\n", Rest)),
        true
    end).
