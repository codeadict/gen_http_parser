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
%% SETTINGS Packet
%% -------------------------------------------------------------------

prop_settings_frame_with_parameters() ->
    ?FORALL(
        Params,
        [
            {header_table_size, pos_integer()},
            {enable_push, boolean()},
            {max_concurrent_streams, non_neg_integer()},
            {initial_window_size, pos_integer()},
            {max_frame_size, pos_integer()},
            {max_header_list_size, pos_integer()},
            {enable_connect_protocol, boolean()}
        ],
        begin
            ?assertRoundTrip(#settings{stream_id = 0, flags = 16#01, params = Params}),
            true
        end
    ).

%% -------------------------------------------------------------------
%% PUSH_PROMISE Packet
%% -------------------------------------------------------------------

prop_push_promise_frame_no_padding() ->
    ?FORALL(
        {StreamId, PromisedStreamId, Hbf},
        {non_zero_stream_id(), non_zero_stream_id(), binary()},
        begin
            ?assertRoundTrip(#push_promise{
                stream_id = StreamId,
                flags = 16#00,
                promised_stream_id = PromisedStreamId,
                hbf = Hbf,
                padding = undefined
            }),
            true
        end
    ).

prop_push_promise_frame_with_padding() ->
    ?FORALL(
        {StreamId, PromisedStreamId, Hbf, Padding},
        {non_zero_stream_id(), non_zero_stream_id(), binary(), binary()},
        begin
            ?assertRoundTrip(#push_promise{
                stream_id = StreamId,
                flags = 16#08,
                promised_stream_id = PromisedStreamId,
                hbf = Hbf,
                padding = Padding
            }),
            true
        end
    ).

%% -------------------------------------------------------------------
%% PING Packet
%% -------------------------------------------------------------------

prop_ping_frame() ->
    ?FORALL(Data, binary(8), begin
        ?assertRoundTrip(#ping{stream_id = 0, flags = 16#00, opaque_data = Data}),
        true
    end).

%% -------------------------------------------------------------------
%% GOAWAY Packet
%% -------------------------------------------------------------------

prop_goaway_frame() ->
    ?FORALL({StreamId, ErrorCode, DebugData}, {non_zero_stream_id(), error_code(), binary()}, begin
        ?assertRoundTrip(#goaway{
            stream_id = 0,
            flags = 16#00,
            last_stream_id = StreamId,
            error_code = ErrorCode,
            debug_data = DebugData
        }),
        true
    end).

%% -------------------------------------------------------------------
%% WINDOW_UPDATE Packet
%% -------------------------------------------------------------------

prop_window_update_frame() ->
    ?FORALL({StreamId, Wsi}, {oneof([0, non_zero_stream_id()]), pos_integer()}, begin
        ?assertRoundTrip(#window_update{
            stream_id = StreamId,
            flags = 16#00,
            window_size_increment = Wsi
        }),
        true
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
    ?LET(
        X,
        pos_integer(),
        begin
            X * 2 + 1
        end
    ).

error_code() ->
    proper_types:oneof([
        no_error,
        protocol_error,
        internal_error,
        flow_control_error,
        settings_timeout,
        stream_closed,
        frame_size_error,
        refused_stream,
        cancel,
        compression_error,
        connect_error,
        enhance_your_calm,
        inadequate_security,
        http_1_1_required,
        {custom_error, 16#11},
        {custom_error, 16#FF},
        {custom_error, 70007}
    ]).
