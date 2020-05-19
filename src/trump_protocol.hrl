%%
%% 报文类型
%%--------------------------------------

%%
-define(PING, 0).
%%
-define(PONG, 1).
%%
-define(CONNECT, 2).
%%
-define(CONNECT_OK, 3).
%%
-define(CONNECT_FAIL, 4).
%%
-define(AUTH, 5).
%%
-define(AUTH_OK, 6).
%%
-define(AUTH_FAIL, 7).
%%
-define(SEND, 8).
%%
-define(SEND_OK, 9).
%%
-define(SEND_FAIL, 10).
%%
-define(PUBLISH, 11).
%%
-define(PUBLISH_OK, 12).
%%
-define(PUBLISH_FAIL, 13).
%%
-define(CREATE_GROUP, 14).
%%
-define(CREATE_GROUP_OK, 15).
%%
-define(CREATE_GROUP_FAIL, 16).
%%
-define(REMOVE_GROUP, 17).
%%
-define(REMOVE_GROUP_OK, 18).
%%
-define(REMOVE_GROUP_FAIL, 19).
%%
-define(JOIN_GROUP, 20).
%%
-define(JOIN_GROUP_OK, 21).
%%
-define(JOIN_GROUP_FAIL, 22).
%%
-define(EXIT_GROUP, 23).
%%
-define(EXIT_GROUP_OK, 24).
%%
-define(EXIT_GROUP_FAIL, 25).
%% 广播
-define(CAST, 26).
%%
-define(CAST_OK, 27).
%%
-define(CAST_FAIL, 28).
%% 协议错误
-define(PROTOCOL_ERROR, 29).
%% 被踢下线
-define(KICKOUT, 30).