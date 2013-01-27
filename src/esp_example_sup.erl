%%%------------------------------------------------------------------------------
%%% @copyright (c) 2013, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%    Root supervisor for the erlangsp_app example application.
%%% @since v1.0.0
%%% @end
%%%------------------------------------------------------------------------------
-module(esp_example_sup).
-behaviour(supervisor).

-include("license_and_copyright.hrl").
-author('Jay Nelson <jay@duomark.com>').

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%%--------------------------------------------------------------------
%% API functions
%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).


%%--------------------------------------------------------------------
%% Supervisor callbacks
%%--------------------------------------------------------------------

-type restart() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-type sup_init_return() :: {ok, {restart(), [supervisor:child_spec()]}}.

-spec init({}) -> sup_init_return().

-define(CHILD(__Mod, __Args), {__Mod, {__Mod, start_link, __Args}, permanent, 5000, worker, [__Mod]}).

init({}) ->
    Epmd_Service = ?CHILD(esp_epmd, [14369, 10]),
    {ok, { {one_for_one, 5, 10}, [Epmd_Service]} }.

