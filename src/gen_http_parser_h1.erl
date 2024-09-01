-module(gen_http_parser_h1).

-export([
    ignore_until_eol/1,
    decode_response_status_line/1,
    decode_response_header/1,
    encode_chunk/1,
    encode_request/4
]).

-define(IS_DIGIT(Val), Val >= $0 andalso Val =< $9).
-define(IS_ALPHA(Val), Val >= $A andalso Val =< $Z orelse Val >= $a andalso Val =< $z).
-define(IS_HEX(Val),
    (?IS_DIGIT(Val) orelse (Val >= $A andalso Val =< $F) orelse (Val >= $a andalso Val =< $f))
).

%%%=========================================================================
%%%  API
%%%=========================================================================

encode_request(Method, Target, Headers, Body) ->
    try
        Body1 = [
            encode_request_line(Method, Target),
            encode_headers(Headers),
            "\r\n",
            encode_body(Body)
        ],
        {ok, Body1}
    catch
        throw:{gen_http, Reason}:_Stacktrace ->
            {error, Reason}
    end.

encode_chunk(eof) ->
    "0\r\n\r\n";
encode_chunk({eof, TrailingHeaders}) ->
    ["0\r\n", encode_headers(TrailingHeaders), "\r\n"];
encode_chunk(Chunk) ->
    Length = erlang:iolist_size(Chunk),
    [integer_to_binary(Length, 16), "\r\n", Chunk, "\r\n"].

decode_response_status_line(Data) ->
    case erlang:decode_packet(http_bin, Data, []) of
        {ok, {http_response, Version, Status, Reason}, Rest} ->
            {ok, {Version, Status, Reason}, Rest};
        {ok, _Other, _Rest} ->
            error;
        {more, _Length} ->
            more;
        {error, _Reason} ->
            error
    end.

decode_response_header(Data) ->
    case erlang:decode_packet(httph_bin, Data, []) of
        {ok, {http_header, _, Name, _Reserved, Value}, Rest} ->
            {ok, {header_name(Name), Value}, Rest};
        {ok, http_eoh, Rest} ->
            {ok, eof, Rest};
        {ok, _Other, _Rest} ->
            error;
        {more, _Length} ->
            more;
        {error, _Reason} ->
            error
    end.

ignore_until_eol(<<>>) ->
    more;
ignore_until_eol(<<"\r\n", Rest/binary>>) ->
    {ok, Rest};
ignore_until_eol(<<_Char, Rest/binary>>) ->
    ignore_until_eol(Rest).

%%%===================================================================
%%% Internal functions
%%%===================================================================

header_name(Name) when is_atom(Name) ->
    string:lowercase(atom_to_binary(Name, utf8));
header_name(Name) when is_binary(Name) ->
    string:lowercase(Name).

encode_request_line(Method, Target) ->
    validate_target(Target),
    [Method, "\s", Target, " HTTP/1.1\r\n"].

encode_headers(Headers) ->
    lists:foldl(
        fun({Name, Value}, Acc) ->
            validate_header_name(Name),
            validate_header_value(Name, Value),
            [Acc, Name, ": ", Value, "\r\n"]
        end,
        "",
        Headers
    ).

encode_body(undefined) -> "";
encode_body(stream) -> "";
encode_body(Body) -> Body.

validate_target(Target) -> validate_target(Target, Target).

validate_target(<<$%, Char1, Char2, Rest/binary>>, OriginalTarget) when
    ?IS_HEX(Char1) andalso ?IS_HEX(Char2)
->
    validate_target(Rest, OriginalTarget);
validate_target(<<Char, Rest/binary>>, OriginalTarget) ->
    case is_reserved(Char) orelse is_unreserved(Char) of
        true ->
            validate_target(Rest, OriginalTarget);
        false ->
            throw({gen_http, {invalid_request_target, OriginalTarget}})
    end;
validate_target(<<>>, _OriginalTarget) ->
    ok.

validate_header_name(Name) ->
    [
        case is_tchar(Char) of
            true -> ok;
            false -> throw({gen_http, {invalid_header_name, Name}})
        end
     || <<Char>> <= Name
    ],
    ok.

validate_header_value(Name, Value) ->
    [
        case is_vchar(Char) orelse Char =:= $\s orelse Char =:= $\t of
            true -> ok;
            false -> throw({gen_http, {invalid_header_value, Name, Value}})
        end
     || <<Char>> <= Name
    ],
    ok.

%%------------------------------------------------------------------------------------------
%% As specified in [RFC 3986, section 2.3](https://tools.ietf.org/html/rfc3986#section-2.3),
%% the following characters are unreserved:
%%
%%   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
%%
%%------------------------------------------------------------------------------------------
-spec is_unreserved(char()) -> boolean().
is_unreserved($-) -> true;
is_unreserved($.) -> true;
is_unreserved($_) -> true;
is_unreserved($~) -> true;
is_unreserved(Char) when ?IS_ALPHA(Char) orelse ?IS_DIGIT(Char) -> true;
is_unreserved(_) -> false.

%%--------------------------------------------------------------------------------------------------
%%  Return true if input char is reserved.
%%
%%  As specified in:
%%
%%  [RFC 3986, Chapter 2.2. Reserved Characters](https://tools.ietf.org/html/rfc3986#section-2.2)
%%
%%   reserved    = gen-delims / sub-delims
%%
%%   gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
%%
%%   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
%%               / "*" / "+" / "," / ";" / "="
%%
%%--------------------------------------------------------------------------------------------------
-spec is_reserved(char()) -> boolean().
is_reserved($:) -> true;
is_reserved($/) -> true;
is_reserved($?) -> true;
is_reserved($#) -> true;
is_reserved($[) -> true;
is_reserved($]) -> true;
is_reserved($@) -> true;
is_reserved($!) -> true;
is_reserved($$) -> true;
is_reserved($&) -> true;
is_reserved($') -> true;
is_reserved($() -> true;
is_reserved($)) -> true;
is_reserved($*) -> true;
is_reserved($+) -> true;
is_reserved($,) -> true;
is_reserved($;) -> true;
is_reserved($=) -> true;
is_reserved(_) -> false.

is_tchar(Char) when ?IS_ALPHA(Char) orelse ?IS_DIGIT(Char) -> true;
is_tchar($!) -> true;
is_tchar($#) -> true;
is_tchar($$) -> true;
is_tchar($%) -> true;
is_tchar($&) -> true;
is_tchar($') -> true;
is_tchar($*) -> true;
is_tchar($+) -> true;
is_tchar($-) -> true;
is_tchar($.) -> true;
is_tchar($^) -> true;
is_tchar($_) -> true;
is_tchar($`) -> true;
is_tchar($|) -> true;
is_tchar($~) -> true;
is_tchar(_) -> false.

is_vchar(Char) when Char >= 33 andalso Char =< 126 ->
    true;
is_vchar(_) ->
    false.
