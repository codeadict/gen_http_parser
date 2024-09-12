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

% encode_with_invalid_header_value_test() ->
%     ?assertEqual(
%         {error, {invalid_header_value, <<"foo">>, <<"b ar">>}},
%         gen_http_parser_h1:encode_request("GET", "/", [{"foo", "b ar"}], undefined)
%     ).


encode_request_with_body_test() ->
    ?assertEqual(
        <<"GET / HTTP/1.1\r\n\r\nBODY">>,
        encode_request("GET", "/", [], "BODY")
    ).


encode_request(Method, Target, Headers, Body) ->
    {ok, IoData} = gen_http_parser_h1:encode_request(Method, Target, Headers, Body),
    iolist_to_binary(IoData).
