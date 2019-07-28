/* Initial beliefs and rules */

// initially, I believe that there is some beer in the fridge
available(beer,fridge).

// owner is believed to be well initially.
owner_well.

// ask the owner for wellbeing.
// ask_owner.

// my owner should not consume more than 2 beers a day :-)
limit(beer,2).

too_much(B) :-
   time.check(YY,MM,DD,_,_,_) &
   .count(consumed(YY,MM,DD,_,_,_,B),QtdB) &
   limit(B,Limit) &
   QtdB >= Limit.
//   QtdB > Limit.


/* Initial goals */

!check_owner_ok.  // initial goal: check that the owner is ok

/* Plans */

+!has(owner,beer)
   :  available(beer,fridge) & not too_much(beer) & owner_well
   <- !at(robot,fridge);
      open(fridge);
      get(beer);
      close(fridge);
      !at(robot,owner);
      hand_in(beer);
      ?has(owner,beer);
      // remember that another beer has been consumed
      time.check(YY,MM,DD,HH,NN,SS);
       //.count(consumed(YY,MM,DD,_,_,_,_),QtB);
      //.print("Not enough beer: year=",YY," month=",MM," day=",DD," numBeers consumed=",QtB);
      +consumed(YY,MM,DD,HH,NN,SS,beer).

+!has(owner,beer)
   :  not available(beer,fridge)
   <- .send(supermarket, achieve, order(beer,5));
      !at(robot,fridge). // go to fridge and wait there.

+!has(owner,beer)
   :  too_much(beer) & limit(beer,L)
   <- .concat("The Department of Health does not allow me to give you more than ", L,
              " beers a day! I am very sorry about that!",M);
      //time.check(YY,MM,DD,HH,NN,SS);	      
       //.count(consumed(YY,MM,DD,_,_,_,_),QtB);
      //.print("Too much beer: year=",YY," month=",MM," day=",DD," numBeers consumed=",QtB);
      .send(owner,tell,msg(M)).


-!has(_,_)
   :  true
   <- .current_intention(I);
      .print("Failed to achieve goal '!has(_,_)'. Current intention is: ",I).

+!at(robot,P) : at(robot,P) <- true.
+!at(robot,P) : not at(robot,P)
  <- move_towards(P);
     !at(robot,P).

// when the supermarket makes a delivery, try the 'has' goal again
+delivered(beer,_Qtd,_OrderId)[source(supermarket)]
  :  true
  <- +available(beer,fridge);
     !has(owner,beer).

// when the fridge is opened, the beer stock is perceived
// and thus the available belief is updated
+stock(beer,0)
   :  available(beer,fridge)
   <- -available(beer,fridge).
+stock(beer,N)
   :  N > 0 & not available(beer,fridge)
   <- -+available(beer,fridge).

+?time(T1,T2,T3,T4,T5,T6) : true
  <-  time.check(T1,T2,T3,T4,T5,T6).

+!check_owner_ok : ask_owner
  <-
    !check_owner_ok(0).

+!check_owner_ok : not ask_owner.

+!check_owner_ok(X) : true
   <-
       if ( X > 0.5 ) {
         !!waitForAnswer(X);
         .send(owner, askOne, are_you_ok, R);
         +owner_is_ok(X);
       }
       .wait(1000);
      !check_owner_ok(X+1).

+!waitForAnswer(X) : true
  <-
     .print("waitForAnswer: ",X);
     .wait(500);
     if ( not owner_is_ok(X) ) {
       .print("waitForAnswer: ",X," no answer");
       -owner_well;
     }.
     
     
    