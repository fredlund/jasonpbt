-module(domestic).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-define(MAX_BEERS_IN_A_DAY,2).
-define(NUMBER_SIPS_IN_A_BEER,2).
-define(NO_WAIT_TIME,500).
-define(WAIT_TIME,10000).


%%-define(debug,true).

-ifdef(debug).
-define(LOG(X,Y),
        io:format("{~p,~p}: ~s~n", [?MODULE,?LINE,io_lib:format(X,Y)])).
-else.
-define(LOG(X,Y),true).
-endif.


%% For now lets assume that all consumption is within one day

-record(state,{node=void,beers=0,sips=0,handed_in=0}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initial_state() -> 
  #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_pre(State) ->
  State#state.node==void.

start_args(_State) ->
  [].

start() ->
  utils:spawn_mas("DomesticRobot.mas2j"),
  java_node:passive_connect
    ([
      {java_exception_as_value,true},
      {call_timeout,20000}
     ]).

start_next(State,Var,_) ->
  State#state{node={call,?MODULE,node,[Var]}}.

start_post(_State,_,Result) ->
  case Result of
    {ok,_} ->
      true;
    _ ->
      false
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sip_pre(State) ->
  State#state.node=/=void.

sip_args(State) ->
  [State#state.node, sip_should_return(State)].

sip_pre(State,[NodeId,ShouldReturn]) ->
  (State#state.node==NodeId)
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

sip_next(State,_Var,_) ->
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
  end.

sip_post(State,[NodeId|_],Result) ->
  io:format
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
  expect_eq(NodeId,"sip()", Result, ExpectedValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

achieveBeer_pre(State) ->
  State#state.node=/=void.

achieveBeer_args(State) ->
  [State#state.node, achieveBeer_should_return(State)].

achieveBeer_pre(State,[NodeId,ShouldReturn]) ->
  (State#state.node==NodeId)
  andalso (ShouldReturn==achieveBeer_should_return(State)).

achieveBeer(Node,ShouldReturn) ->
  catch 
    begin
      utils:achieve(Node,"owner","robot","has(owner,beer)"),
      get_first_message(Node, "owner", ShouldReturn)
    end.

achieveBeer_next(State,_Var,_) ->
  case too_many_beers(State) of
    true -> 
      State;
    false -> 
      State#state
	{
	beers = State#state.beers+1, 
	handed_in = State#state.handed_in+1
	}
  end.

achieveBeer_should_return(State) ->
  no_beer(State) or too_many_beers(State).

no_beer(State) ->
  (State#state.beers == 0) andalso (State#state.sips == 0).
  
too_many_beers(State) ->
  State#state.handed_in >= ?MAX_BEERS_IN_A_DAY.

achieveBeer_post(State,[NodeId|_],Result) ->
  io:format("achieveBeer_post(~p)~n",[Result]),
  NoBeer = no_beer(State),
  TooManyBeers = too_many_beers(State),
  ExpectedValue =
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
  expect_eq(NodeId,"achieveBeer", Result, ExpectedValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

weight(_State,Command) ->
  case Command of
    sip -> 3;
    _ -> 1
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node({ok,NodeId}) ->
  NodeId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expect_eq(NodeId,Context,PreResult,Recognizer) ->
  Result = strip_annotations(PreResult),
  ?LOG("ee(~p,~p,~p)~n",[Context,Result,Recognizer]),
  case {Result,Recognizer} of
    {[],void} ->
      true;
    {[],_} ->
      io:format
        ("~n*** Error: ~p: expected a reply but received nothing (~p)~n",
         [Context,Recognizer]),
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
            ("~n*** Error: ~p: returned result of wrong shape ~p (~p)~n",
             [Context,Result,Recognizer]),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_first_message(NodeId, Agent, ShouldReturn) ->
  Timeout =
    if
      ShouldReturn -> ?WAIT_TIME;
      true -> ?NO_WAIT_TIME
    end,
  utils:first_waiting_message(NodeId, Agent, Timeout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop() ->
  ?FORALL(Cmds,eqc_statem:commands(?MODULE),
    begin
      HSR = {_H,_S,Res} = eqc_statem:run_commands(?MODULE,Cmds),
      eqc_statem:pretty_commands(?MODULE, Cmds, HSR, Res == ok),
      Res == ok
    end).
      
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

      

		      
  
  


  
