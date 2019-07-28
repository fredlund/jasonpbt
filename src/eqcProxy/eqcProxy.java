package eqcProxy;

import java.util.logging.Level;
import java.net.InetAddress;
import java.net.UnknownHostException;


import jason.JasonException;
import jason.NoValueException;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.asSemantics.Message;


public class eqcProxy extends DefaultInternalAction {

  @Override
  public Object execute(TransitionSystem ts, Unifier un, Term[] args)
    throws Exception {
    new Thread(new Server()).start();
    return true;
  }
}

class Server implements Runnable {
  public void run() {
    try {
      long timestamp = System.currentTimeMillis();
      String hostName = InetAddress.getLocalHost().getHostName();
      String nodeName = "mas" + timestamp + "@"+hostName;
      String erlangNodeName = "erlang_mas_connection@"+hostName;
      System.out.println("starting java_erlang server at node "+nodeName);
      javaErlang.JavaErlang.reportAndReceiveConnection(Level.INFO,nodeName,erlangNodeName,"java_node_connection_listener",null,true);
    } catch (Throwable exc) {
      System.out.println("Exception "+exc+" raised");
      exc.printStackTrace();
    }
  }
}
