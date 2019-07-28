-module(test).

-export([start/0, achieveHasBeer/0, sip/0, askTime/0, owner_beliefs/0]).
-export([tellYes/1,tellNo/1]).
-export([waitingMessages/1]).


start() ->
  utils:spawn_mas("DomesticRobot.mas2j"),
  {ok,NodeId} = java_node:passive_connect([{call_timeout,20000}]),
  put(nodeId,NodeId).

achieveHasBeer() ->
  utils:achieve(get(nodeId),"owner","robot","has(owner,beer)").

sip() ->
  utils:environment_action(get(nodeId),"owner","sip(beer)").

askTime() ->
  utils:askOne(get(nodeId),"owner","robot","time(_,_,_,_,_,_)").
		   
owner_beliefs() ->
  utils:belief_base(get(nodeId),"owner").

tellYes(ReplyId) ->
  utils:tell(get(nodeId),"owner","robot","yes",ReplyId).

tellNo(ReplyId) ->
  utils:tell(get(nodeId),"owner","robot","no",ReplyId).

waitingMessages(Agent) ->  
  utils:waiting_messages(get(nodeId),Agent,0).

