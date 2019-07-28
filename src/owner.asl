/* Initial goals */

// Which facts correspond to perceptions
isPerception(has(owner,beer)).

// Name of agent
agentName(owner).

!start.

+!start : true
  <- eqcProxy.eqcProxy.

@envAction
+!kqml_received(self, envAction, Action, _)
   :  true
   <-
        .print("got envAction ",Action);
        Action.

@kqmlReceive
+!kqml_received(Sender, Command, Arg1, Arg2)
   :  not Arg1 = envAction(_) & agentName(AgentName)
   <-
        .print("got: ",Sender,",",Command,",",Arg1,",",Arg2);
        eqcProxy.toProxy(AgentName,comm(Sender,Command,Arg1,Arg2)).

+!envAction(X)[_] : true
  <-
        .print("got envAction: ",X);
        X.

+!X : not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <-
        .print("got !X: ",X);
        eqcProxy.toProxy(AgentName, posAchieve(X)).

-!X : not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <- eqcProxy.toProxy(AgentName, negAchieve(X)).

+?X : not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <- eqcProxy.toProxy(AgentName, posTest(X)).

-?X : not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <- eqcProxy.toProxy(AgentName, negTest(X)).

@keepPerceptionBeliefs[atomic]
+X [source(A)]: A \== self & isPerception(X) & not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <-
     .print("Keeping new perception belief ",X," from ",A);
     eqcProxy.toProxy(AgentName, posFact(X)).

@deleteAssertedBeliefs[atomic]
+X [source(A)]: A \== self & not isPerception(X) & not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <- +deleted(X,A);
     .print("Deleting new non-perception belief ",X," from ",A);
     -X [source(A)];
     eqcProxy.toProxy(AgentName, posFact(X)).

@forwardNegativeBelief[atomic]
-X [source(A)] : A \== self & not deleted(X,A) & not X = kqml_received(_,_,_,_) & agentName(AgentName)
  <- .print("Sending negative belief ",X," from ",A," to eqc");
     eqcProxy.toProxy(AgentName, negFact(X)).

@dontForwardNegativeBelief[atomic] 
-X [source(A)] : A \== self & deleted(X,A) & not X = kqml_received(_,_,_,_)
  <- .print("Skipping deleted belief ",X," from ",A);
     -deleted(X,A).

