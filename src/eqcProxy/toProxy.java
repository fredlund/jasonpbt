package eqcProxy;

import jason.JasonException;
import jason.NoValueException;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.asSemantics.Message;
import jason.infra.centralised.*;


public class toProxy extends DefaultInternalAction {
  @Override
  public int getMinArgs() {
    return 2;
  }
  
  @Override
  public int getMaxArgs() {
    return 2;
  }

  @Override
  public Object execute(TransitionSystem ts, Unifier un, Term[] args)
    throws Exception {
    checkArguments(args);

    if (args[0].isAtom()) {
      Proxy.put(args[0].toString(),args[1]);
      return true;
    } else {
      throw JasonException.createWrongArgument(this,"first argument '"+args[0]+"' must be an atom.");
    }
  }
}

