-module(gen_http_parser_h2_test).

-include_lib("eunit/include/eunit.hrl").

set_flags_test() ->
    ?assertEqual(
        16#01,
        gen_http_parser_h2:set_flags(ping, [ack])
    ),
    ?assertEqual(
        16#01,
        gen_http_parser_h2:set_flags(data, [end_stream])
    ),
    ?assertException(
        error,
        function_clause,
        gen_http_parser_h2:set_flags(data, [ack])
    ).

set_flags_with_opts_test() ->
    ?assertEqual(
        16#01 bor 16#08,
        gen_http_parser_h2:set_flags(16#01, data, [padded])
    ),
    ?assertException(
        error,
        function_clause,
        gen_http_parser_h2:set_flags(16#00, data, [ack])
    ).

is_flag_set_test() ->
    ?assertEqual(
        true,
        gen_http_parser_h2:is_flag_set(16#08, data, padded)
    ),
    ?assertEqual(
        false,
        gen_http_parser_h2:is_flag_set(16#00, data, padded)
    ),
    ?assertException(
        error,
        function_clause,
        gen_http_parser_h2:is_flag_set(16#00, data, ack)
    ).

decode_next_with_incomplete_frame_test() ->
    ?assertEqual(
        more,
        gen_http_parser_h2:decode_next(<<>>)
    ).

%% -------------------
%% DATA frame tests
%% -------------------

data_with_bad_padding_test() ->
    %% the pad length is >= payload byte size
    Payload = <<6:8, "hello">>,
    DbgData = <<"the padding length of a data frame is bigger than the payload length">>,
    Packet = list_to_binary(gen_http_parser_h2:encode_raw(16#00, 16#08, 3, Payload)),
    ?assertEqual(
        {error, {protocol_error, DbgData}},
        gen_http_parser_h2:decode_next(Packet)
    ).
