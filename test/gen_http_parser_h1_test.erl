-module(gen_http_parser_h1_test).

-include_lib("eunit/include/eunit.hrl").

encode_with_header_test() ->
    ?assertEqual(
        <<"GET / HTTP/1.1\r\nfoo: bar\r\n\r\n">>,
        encode_request("GET", "/", [{"foo", "bar"}], undefined)
    ).

encode_with_invalid_header_name_test() ->
    ?assertEqual(
        {error, {invalid_header_name, <<"f oo">>}},
        gen_http_parser_h1:encode_request("GET", "/", [{"f oo", "bar"}], undefined)
    ).

encode_with_invalid_header_value_test() ->
    ?assertEqual(
        {error, {invalid_header_value, <<"foo">>, <<"bar\r\n">>}},
        gen_http_parser_h1:encode_request("GET", "/", [{"foo", "bar\r\n"}], undefined)
    ).

encode_with_invalid_target_test() ->
    Cases = [
        <<"/ /">>,
        <<"/%foo">>,
        <<"/foo%x">>
    ],
    [
        ?assertEqual(
            {error, {invalid_request_target, Target}},
            gen_http_parser_h1:encode_request("GET", Target, [], undefined)
        )
     || Target <- Cases
    ].

encode_with_valid_target_test() ->
    ?assertEqual(
        <<"GET /foo%20bar HTTP/1.1\r\n\r\n">>,
        encode_request("GET", "/foo%20bar", [], undefined)
    ).

encode_request_with_body_test() ->
    ?assertEqual(
        <<"GET / HTTP/1.1\r\n\r\nBODY">>,
        encode_request("GET", "/", [], "BODY")
    ).

encode_request_with_body_and_headers_test() ->
    ?assertEqual(
        <<"GET / HTTP/1.1\r\nfoo: bar\r\n\r\nBODY">>,
        encode_request("GET", "/", [{"foo", "bar"}], "BODY")
    ).

encode_chunk_with_eof_test() ->
    ?assertEqual(
        "0\r\n\r\n",
        gen_http_parser_h1:encode_chunk(eof)
    ).

encode_chunk_with_iodata_test() ->
    Cases = [
        {"foo", <<"3\r\nfoo\r\n">>},
        {["hello ", $w, [[<<"or">>], $l], $d], <<"B\r\nhello world\r\n">>}
    ],

    [
        ?assertEqual(Expected, iolist_to_binary(gen_http_parser_h1:encode_chunk(Input)))
     || {Input, Expected} <- Cases
    ].

encode_request(Method, Target, Headers, Body) ->
    {ok, IoData} = gen_http_parser_h1:encode_request(Method, Target, Headers, Body),
    iolist_to_binary(IoData).
