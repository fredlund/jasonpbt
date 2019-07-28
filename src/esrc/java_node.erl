-module(java_node).

%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
        io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.

-export([active_connect/2,passive_connect/1,node/0]).

active_connect(Name,Options) ->
 try
    case jt_storage:read(java_node) of
      false -> ok;
      {_,NodeId} -> java:terminate(NodeId)
    end
  catch _:_ ->
      ok
  end,
  {ok,NewNodeId} = java:connect(Name,Options),
  jt_storage:write(java_node,NewNodeId),
  {ok,NewNodeId}.

setup_passive_connect() ->
  case whereis(java_node_connection_listener) of
    undefined ->
      spawn(fun () ->
                register(java_node_connection_listener,self()),
                wait_for_nodes()
            end);
    _ ->
      ok
  end.

passive_connect(Options) ->
  setup_passive_connect(),
  try
    case storage:read(java_node) of
      false -> 
	ok;
      {_,NodeId} -> 
	java:terminate(NodeId),
	timer:sleep(500)
    end
  catch _:_ ->
      ok
  end,
  java_node_connection_listener ! {get_new_node, self()},
  receive
    {new_node, NodeName} ->
      ?LOG("java_connection to ~p~n",[NodeName]),
      {ok,NewNodeId} = java:connect(NodeName,Options),
      ?LOG("returning node_id ~p~n",[NewNodeId]),
      storage:write(java_node,NewNodeId),
      {ok,NewNodeId}
  end.

node() ->
  {_,NodeId} = storage:read(java_node),
  NodeId.

wait_for_nodes() ->
  wait_for_nodes(void,void).
wait_for_nodes(Listener,NodeName) ->
  receive
    {get_new_node,Pid} ->
      if
        NodeName =/= void ->
          Listener ! {new_node, NodeName},
          wait_for_nodes(void, void);
        true ->
          wait_for_nodes(Pid, NodeName)
      end;
    NewNodeName when is_list(NewNodeName) ->
      if
        Listener =/= void ->
          Listener ! {new_node, list_to_atom(NewNodeName)},
          wait_for_nodes(void, void);
        true ->
          wait_for_nodes(Listener, list_to_atom(NewNodeName))
      end
  end.

