-module(prop_gen_http_parser_h2).

-include("gen_http_h2_frames.hrl").

-include_lib("proper/include/proper.hrl").

-include_lib("stdlib/include/assert.hrl").

-define(assertRoundTrip(Frame), begin
    Encoded = list_to_binary(gen_http_parser_h2:encode(Frame)),
    ?assertEqual(
        {ok, Frame, <<"rest">>},
        gen_http_parser_h2:decode_next(<<Encoded/binary, "rest">>)
    )
end).

%% -------------------------------------------------------------------
%% CONTINUATION Packet
%% -------------------------------------------------------------------

prop_continuation_frame() ->
    ?FORALL({StreamId, Hbf}, {non_zero_stream_id(), binary()}, begin
        ?assertRoundTrip(#continuation{stream_id = StreamId, hbf = Hbf}),
        true
    end).

-spec non_zero_stream_id() -> proper_types:type().
non_zero_stream_id() ->
    pos_integer().
