-module(usr_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  start_link("usrDb").

start_link(FileName) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [FileName]).

init(FileName) ->
  UsrChild = {usr_gs, {usr_gs, start_link, [FileName]},
              permanent, 2000, worker, [usr_gs, usr_db]},
  {ok, {{one_for_all, 1, 1}, [UsrChild]}}.
