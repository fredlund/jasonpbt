-module(utils).

-export([spawn_mas/1]).
-export([active_connect_to_mas/1]).
-export([achieve/4, askOne/4, tell/5]).
-export([environment_action/3]).
-export([belief_base/2]).
-export([first_waiting_message/3, waiting_messages/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

active_connect_to_mas(Mas) ->
  CurrentNode = node(),
  [_,HostPart] = string:tokens(atom_to_list(CurrentNode),[$@]),
  MasNode = list_to_atom("mas@"++HostPart),
  spawn_mas(Mas),
  timer:sleep(1000),
  io:format("Trying to connect to mas at node ~p~n",[MasNode]),
  {ok,NodeId} = java_node:active_connect(MasNode,[]), %% {log_level,all}]),
  io:format("Connected to mas node ~p~n",[NodeId]),
  {ok,NodeId}.

spawn_mas(Mas) ->
  %% Command = "../../build/scripts/jason" ++ " " ++ Mas,
  JavaErlangPriv = code:priv_dir(java_erlang),
  OtpErlangPriv = code:priv_dir(jinterface),
  ClassPath =
    "../../build/libs/jason-2.3.jar:bin/classes"
    ++ ":" ++ JavaErlangPriv++"/JavaErlang.jar"
    ++ ":" ++ OtpErlangPriv++"/OtpErlang.jar",
  Command = 
    "java -cp "
    ++ ClassPath
    ++ " jason.infra.centralised.RunCentralisedMAS "
    ++ Mas,
  spawn(fun () -> cmd_with_status(Command) end).

first_waiting_message(NodeId,Agent,Timeout) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'firstMessage',
     [Agent,Timeout]).

waiting_messages(NodeId,Agent,Timeout) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'queuedObjects',
     [Agent,Timeout]).

achieve(NodeId,Sender,Achiever,Goal) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'send',
     ["achieve",Sender,Achiever,parse_term(NodeId,Goal)]).

askOne(NodeId,Sender,Achiever,Goal) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'send',
     ["askOne",Sender,Achiever,parse_term(NodeId,Goal)]).

tell(NodeId,Sender,Tellee,Tell,ReplyId) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'send',
     ["tell",Sender,Tellee,parse_term(NodeId,Tell),ReplyId]).

environment_action(NodeId,Agent,Action) ->
  achieve(NodeId,Agent,Agent,lists:flatten("envAction("++Action++")")).

belief_base(NodeId,Agent) ->
  java:call_static
    (NodeId,
     'eqcProxy.Proxy',
     'beliefBase',
     [Agent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_term(NodeId,List) ->
  String = java:list_to_string(NodeId,List),
  java:call_static
    (NodeId,
     'jason.asSyntax.ASSyntax',
     parseLiteral,
     [String]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cmd_with_status(Command) ->
  Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status, stderr_to_stdout]),
  get_data(Port).

get_data(Port) ->
    receive
    {Port, {data, Bytes}} ->
	io:format("~s~n",[Bytes]),
        get_data(Port);
      {Port, eof} ->
        Port ! {self(), close},
        receive
        {Port, closed} ->
            true
        end,
        receive
        {'EXIT',  Port,  _} ->
            ok
        after 1 ->              % force context switch
            ok
        end,
        ExitCode =
            receive
            {Port, {exit_status, Code}} ->
                Code
        end,
        ExitCode
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

