-ifndef(H2_FRAMES_HRL).
-define(H2_FRAMES_HRL, true).

-define(PACKET_DATA, 0).
-define(PACKET_HEADERS, 1).
-define(PACKET_PRIORITY, 2).
-define(PACKET_RST_STREAM, 3).
-define(PACKET_SETTINGS, 4).
-define(PACKET_PUSH_PROMISE, 5).
-define(PACKET_PING, 6).
-define(PACKET_GOAWAY, 7).
-define(PACKET_WINDOW_UPDATE, 8).
-define(PACKET_CONTINUATION, 9).

-record(data, {stream_id, flags = 16#0, data, padding}).
-record(headers, {stream_id, flags = 16#0, is_exclusive, stream_dependency, weight, hbf, padding}).
-record(priority, {stream_id, flags = 16#0, is_exclusive, stream_dependency, weight}).
-record(rst_stream, {stream_id, flags = 16#0, error_code}).
-record(settings, {stream_id, flags = 16#0, params}).
-record(push_promise, {stream_id, flags = 16#0, promised_stream_id, hbf, padding}).
-record(ping, {stream_id, flags = 16#0, opaque_data}).
-record(goaway, {stream_id, flags = 16#0, last_stream_id, error_code, debug_data}).
-record(window_update, {stream_id, flags = 16#0, window_size_increment}).
-record(continuation, {stream_id, flags = 16#0, hbf}).
-record(unknown, {}).

-type data() :: #data{}.
-type headers() :: #headers{}.
-type priority() :: #priority{}.
-type rst_stream() :: #rst_stream{}.
-type settings() :: #settings{}.
-type push_promise() :: #push_promise{}.
-type ping() :: #ping{}.
-type goaway() :: #goaway{}.
-type window_update() :: #window_update{}.
-type continuation() :: #continuation{}.
-type unknown() :: #unknown{}.
-type packet() ::
    data()
    | headers()
    | priority()
    | rst_stream()
    | settings()
    | push_promise()
    | ping()
    | goaway()
    | window_update()
    | continuation()
    | unknown().
-endif.
