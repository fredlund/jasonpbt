-module(domestic_timed_dynamic).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
%%-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eqc/include/eqc_component.hrl").
-include_lib("eqc/include/eqc_dynamic_cluster.hrl").

-define(MAX_BEERS_IN_A_DAY,2).
-define(NUMBER_SIPS_IN_A_BEER,2).
-define(NO_WAIT_TIME,500).
-define(WAIT_TIME,10000).

-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
        io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


%% For now lets assume that all consumption is within one day

-record(state,
        {
          node=void,beers=0,sips=0,handed_in=0,postponed=void,replyId
        }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_state() -> 
  #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  State#state.node==void.

start_args(_State) ->
  [].

start() ->
  catch
    begin
      spawn(fun () -> 
                timer:sleep(1000), 
                utils:spawn_mas("DomesticRobot.mas2j")
            end),
      {ok,NodeId} =
	java_node:passive_connect
	  ([
            %%{log_level,all},
            %%{java_verbose,"FINER"},
	    {java_exception_as_value,true},
	    {call_timeout,20000}
	   ]),
      io:format("connected: node = ~p!~n",[NodeId]),
      ?SYMBOLIC(NodeId)
    end.

start_next(State,Var,_) ->
  State#state{node=Var}.

start_post(_State,_,Result) ->
  case Result of
    {'EXIT',_} ->
      false;
    _ ->
      true
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sip_pre(State) ->
  (State#state.node=/=void)
    andalso (State#state.postponed==void).

sip_args(State) ->
  [State#state.node, sip_should_return(State)].

sip_pre(State,[NodeId,ShouldReturn]) ->
  (NodeId==State#state.node) 
    andalso (ShouldReturn==sip_should_return(State)).

sip(Node,ShouldReturn) ->
  catch 
    begin
      utils:environment_action(Node,"owner","sip(beer)"),
      get_first_message(Node, "owner", ShouldReturn)
    end.

sip_should_return(State) ->
  no_sips(State) orelse last_sip(State).

no_sips(State) ->
  (State#state.sips == 0) andalso (State#state.beers == 0).

last_sip(State) ->
  ((State#state.sips == 1) andalso (State#state.beers == 0)).

sip_next(State,Result,[_,ShouldReturn]) ->
  io:format("sip_next: result is ~p~n",[Result]),
  case is_ok_comm(Result) of
    true ->
      State#state{postponed={sip,State,ShouldReturn},
                  replyId=ok_replyId(Result)};
    false ->
      if
        State#state.sips == 0 ->
          if
            State#state.beers > 0 ->
              State#state
                {
                beers = State#state.beers-1,
                sips = ?NUMBER_SIPS_IN_A_BEER-1
               };
            true -> State
          end;
        true -> State#state{sips = State#state.sips-1}
      end
  end.

sip_post(State,[NodeId|_],Result) ->
  ?LOG
    ("sip_post(~p) sips=~p beers=~p~n",
     [Result,State#state.sips,State#state.beers]),
  LastSip = last_sip(State),
  NoSips = no_sips(State),
  ExpectedValue =
    if
      LastSip ->
	{negFact,{has,owner,beer}};
      NoSips ->
	{negAchieve,{envAction,{sip,beer}}};
      true ->
        void
    end,
  expect_eq(NodeId,"sip()", strip_annotations(Result), ExpectedValue, State).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

achieveBeer_pre(State) ->
  (State#state.node=/=void)
    andalso (State#state.postponed==void).

achieveBeer_args(State) ->
  [State#state.node, achieveBeer_should_return(State)].

achieveBeer_pre(State,[NodeId,ShouldReturn]) ->
  (NodeId==State#state.node) 
  andalso (ShouldReturn==achieveBeer_should_return(State)).

achieveBeer(Node,ShouldReturn) ->
  catch 
    begin
      utils:achieve(Node,"owner","robot","has(owner,beer)"),
      get_first_message(Node, "owner", ShouldReturn)
    end.

achieveBeer_next(State,Result,[_,ShouldReturn]) ->
  io:format("achieveBeer_next: result is ~p~n",[Result]),
  case is_ok_comm(Result) of
    true ->
      State#state{postponed={achieveBeer,State,ShouldReturn},
                  replyId=ok_replyId(Result)};
    false ->
      case too_many_beers(State) of
        true -> 
          State;
        false -> 
          State#state
            {
            beers = State#state.beers+1, 
            handed_in = State#state.handed_in+1
           }
      end
  end.

achieveBeer_should_return(State) ->
  no_beer(State) or too_many_beers(State).

no_beer(State) ->
  (State#state.beers == 0) andalso (State#state.sips == 0).
  
too_many_beers(State) ->
  State#state.handed_in >= ?MAX_BEERS_IN_A_DAY.

achieveBeer_post(State,[NodeId|_],Result) ->
  ?LOG("achieveBeer_post(~p)~n",[Result]),
  NoBeer = no_beer(State),
  TooManyBeers = too_many_beers(State),
  Recognizer =
    if
      TooManyBeers ->
        fun (Term) ->
            case Term of
              {comm,robot,tell,{msg,"The Department of Health does not allow me to give you more than 2 beers a day! I am very sorry about that!"},_} ->
                true;
              _ ->
                false
            end
        end;
      NoBeer ->
	{posFact,{has,owner,beer}};
      true ->
	void
    end,
  expect_eq(NodeId,"achieveBeer", strip_annotations(Result), Recognizer, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timeChange_pre(State) ->
  (State#state.node=/=void)
    andalso (State#state.postponed==void).

timeChange_args(State) ->
  [State#state.node].

timeChange_pre(State,[NodeId]) ->
  (NodeId==State#state.node).

timeChange(Node) ->
  catch
    java:call_static
    (Node,
     'time.check',
     'incDay',
     []).

timeChange_next(State,Var,_) ->
  State#state{handed_in=0}.

timeChange_post(State,[NodeId|_],Result) ->
  case Result of
    {'EXIT',_} -> 
      ?LOG("Result is ~p~n",[Result]),
      false;
    _ -> 
      true
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_ok_pre(State) ->
  (State#state.node=/=void)
    andalso (State#state.postponed=/=void).

is_ok_args(State) ->
  [State#state.node, State#state.replyId, should_return(State)].

is_ok_pre(State,[NodeId,ReplyId,ShouldReturn]) ->
  (NodeId==State#state.node)
    andalso (State#state.replyId==ReplyId)
    andalso (should_return(State)==ShouldReturn).

is_ok(Node,ReplyId,ShouldReturn) ->
  catch
    begin
      utils:tell(Node,"owner","robot","yes",atom_to_list(ReplyId)),
      get_first_message(Node, "owner", ShouldReturn)
    end.

is_ok_next(State,Var,[Node,_,ShouldReturn]) ->
  PrevState = postponed_state(State),
  case postponed_command(State) of
    sip ->
      sip_next(PrevState,Var,[Node,ShouldReturn]);
    achieveBeer ->
      achieveBeer_next(PrevState,Var,[Node,ShouldReturn])
  end.

is_ok_post(State,[Node,_,ShouldReturn],Result) ->
  ?LOG("is_ok_post(~p)~nresult=~p~n",[State,Result]),
  case postponed_command(State) of
    sip ->
      sip_post(State,[Node,ShouldReturn],Result);
    achieveBeer ->
      achieveBeer_post(State,[Node,ShouldReturn],Result)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

should_return(State) ->
  case State#state.postponed of
    {_,_,ShouldReturn} ->
      ShouldReturn
  end.

postponed_command(State) ->
  case State#state.postponed of
    {Command,_,_} ->
      Command
  end.

postponed_state(State) ->
  case State#state.postponed of
    {_,OldState,_} ->
      OldState
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weight(_State,Command) ->
  case Command of
    sip -> 3;
    _ -> 1
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


is_ok_comm([{comm,robot,askOne,are_you_ok,_}]) ->
  true;
is_ok_comm(_) ->
  false.

ok_replyId([{comm,robot,askOne,are_you_ok,ReplyId}]) ->
  ReplyId.

expect_eq(NodeId,Context,Result,Recognizer,State) ->
  ?LOG("ee(~p,~p,~p)~nwith state ~p~n",[Context,Result,Recognizer,State]),
  is_ok_comm(Result) orelse
    case {Result,Recognizer} of
      {[],void} ->
        true;
      {[],_} ->
        io:format
          ("~n*** Error: ~p: expected a reply but received nothing~n",
           [Context]),
        false;
      {[ResultTerm],_} ->
        CheckResult =
          if
            is_function(Recognizer,1) ->
              Recognizer(ResultTerm);
            true ->
              ResultTerm == Recognizer
          end,
        if
          CheckResult -> true;
          true ->
            io:format
              ("~n*** Error: ~p: returned result of wrong shape ~p~n",
               [Context,Result]),
            io:format
              ("~nRobot belief base:~n~p~n",
               [utils:belief_base(NodeId,"robot")]),
            false
        end
      end.

strip_annotations({'$annotation',Term,_Annotation}) ->
  strip_annotations(Term);
strip_annotations(Tuple) when is_tuple(Tuple) ->
  list_to_tuple(strip_annotations(tuple_to_list(Tuple)));
strip_annotations([Hd|Tl]) ->
  [strip_annotations(Hd)|strip_annotations(Tl)];
strip_annotations(Term) ->
  Term.

get_first_message(NodeId, Agent, ShouldReturn) ->
  Timeout =
    if
      ShouldReturn -> ?WAIT_TIME;
      true -> ?NO_WAIT_TIME
    end,
  utils:first_waiting_message(NodeId, Agent, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

api_spec() ->
  #api_spec{}.

prop() ->
  ?FORALL(Cmds,eqc_dynamic_cluster:dynamic_commands(?MODULE),
          ?CHECK_COMMANDS
          (HSR = {_H,_S,Res},
           ?MODULE,
           Cmds,
           begin
             eqc_statem:pretty_commands(?MODULE, Cmds, HSR, Res == ok),
             Res == ok
           end)).
      
sample() ->
  eqc_gen:sample(eqc_statem:commands(?MODULE)).

check() ->
  case eqc:counterexample(prop()) of
    true ->
      ok;
    CounterExample ->
      io:format("~n~nRerunning counterexample:~n"),
      eqc:check(prop(),CounterExample)
  end.

      

		      
  
  


  
