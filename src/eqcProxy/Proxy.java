package eqcProxy;

import jason.JasonException;
import jason.NoValueException;
import jason.asSemantics.*;
import jason.asSyntax.*;
import jason.infra.centralised.*;
import jason.bb.BeliefBase;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import com.ericsson.otp.erlang.*;


public class Proxy {
  private final static Map<String,BlockingQueue<Term>> queues =
    Collections.synchronizedMap(new HashMap<String,BlockingQueue<Term>>());

  public static void send(String messageType, String from, String to, Object message) {
    System.out.println("send("+messageType+","+message+","+to);
    Message msg = new Message(messageType, from, to, message);
    msg.setReceiver(to);
    sendMessage(msg);
  }

  public static void send(String messageType, String from, String to, Object message, String replyId) {
    System.out.println("send("+messageType+","+message+","+to+" replying to "+replyId);
    Message msg = new Message(messageType, from, to, message);
    msg.setReceiver(to);
    msg.setInReplyTo(replyId);
    sendMessage(msg);
  }

  private static void sendMessage(Message msg) {
    BaseCentralisedMAS masRunner = BaseCentralisedMAS.getRunner();
    CentralisedAgArch rec = masRunner.getAg(msg.getReceiver());
    if (rec == null) {
      System.out.println("receiver is "+msg.getReceiver());
      System.out.println("could not find receiver");
      System.out.println("agents:");
      for (String agentName : masRunner.getAgs().keySet()) {
        System.out.println(agentName);
      }
      throw new RuntimeException();
    } else {
      rec.receiveMsg(msg.clone()); // send a cloned message
    }
  }

  public static OtpErlangObject beliefBase(String agent) {
    BaseCentralisedMAS masRunner = BaseCentralisedMAS.getRunner();
    CentralisedAgArch rec = masRunner.getAg(agent);
    if (agent == null) {
      System.out.println("could not find agent "+agent);
      throw new RuntimeException();
    }
    TransitionSystem ts = rec.getTS();
    Agent ag = ts.getAg();
    BeliefBase bb = ag.getBB();
    OtpErlangObject[] beliefs = new OtpErlangObject[bb.size()];
    int i=0;
    for (Literal l : bb) {
      beliefs[i++] = t2e(l);
    }
    return new OtpErlangList(beliefs);
  }

  static BlockingQueue<Term> getQueue(String agent) {
    synchronized (queues) {
      BlockingQueue<Term> queue = queues.get(agent);
      if (queue == null) {
        queue = new LinkedBlockingQueue<Term>();
        queues.put(agent,queue);
      }
      return queue;
    }
  }
  
  static void put(String agent, Term term) {
    BlockingQueue<Term> myQueue = getQueue(agent);
    try {
      myQueue.put(term);
    } catch (InterruptedException sexc) {
      Thread.currentThread().interrupt();
      throw new RuntimeException(sexc);
    }
  }

  public static OtpErlangObject firstMessage(String agent, long timeout) {
    BlockingQueue<Term> myQueue = getQueue(agent);

    //System.out.println("waiting "+timeout);
    try {
      Term firstMessage = myQueue.poll(timeout,TimeUnit.MILLISECONDS);
      if (firstMessage == null) {
	//System.out.println("got null after sleeping");
	return new OtpErlangList();
      } else {
        OtpErlangObject[] objects = new OtpErlangObject[1];
	//System.out.println("got "+firstMessage);
        objects[0] = t2e(firstMessage);
	return new OtpErlangList(objects);
      }
    } catch (InterruptedException exc) {
      System.out.println("interrupted!");
      throw new RuntimeException();
    }
  }

  public static OtpErlangObject queuedObjects(String agent, long timeout) {
    BlockingQueue<Term> myQueue = getQueue(agent);

    //System.out.println("waiting "+timeout);
    try {
      Term firstMessage = myQueue.poll(timeout,TimeUnit.MILLISECONDS);
      if (firstMessage == null) {
	//System.out.println("got null after sleeping");
	return new OtpErlangList();
      } else {
	//System.out.println("got "+firstMessage);
	return messages(myQueue, firstMessage);
      }
    } catch (InterruptedException exc) {
      System.out.println("interrupted!");
      throw new RuntimeException();
    }
  }

  static OtpErlangObject messages(BlockingQueue<Term> myQueue, Term firstMessage) {
    int size = myQueue.size();
    int allocSize = size;
    int startIndex = 0;

    if (firstMessage != null) {
      allocSize = size+1;
      startIndex = 1;
    }
    
    OtpErlangObject[] objects = new OtpErlangObject[allocSize];

    if (firstMessage != null)
      objects[0] = t2e(firstMessage);
    
    try {
      for (int i=startIndex; i<allocSize; i++) {
        objects[i] = t2e(myQueue.take());
      }
    } catch (InterruptedException sexc) {
      Thread.currentThread().interrupt();
      throw new RuntimeException(sexc);
    }
    return new OtpErlangList(objects);
  }

  static OtpErlangObject t2e(Term t) {
    try {
      OtpErlangObject result = t2er(t);
      return result;
    }
    catch (Throwable exc) {
      System.out.println("Could not translate term "+t.toString());
      throw exc;
    }
  }

  static OtpErlangObject t2er(Term t) {
    if (t.isPlanBody()) {
      System.out.println("Term "+t+" is a plan body; cannot handle that yet");
      throw new RuntimeException();
    } else if (t.isRule()) {
      Rule r = (Rule) t;
      Term head = r.getHead();
      Term body = r.getBody();
      OtpErlangObject[] args = new OtpErlangObject[3];
      args[0] = new OtpErlangAtom("rule");
      args[1] = t2er(head);
      args[2] = t2er(body);
      return new OtpErlangTuple(args);
    } else if (t.isPred()) {
      Structure s = (Structure) t;
      OtpErlangObject se = structure2e(s);
      Pred p = (Pred) t;
      ListTerm l = p.getAnnots();
      if (l == null) return se;
      else {
	OtpErlangObject ae = t2er(l);
	OtpErlangObject[] args = new OtpErlangObject[3];
	args[0] = new OtpErlangAtom("$annotation");
	args[1] = se;
	args[2] = ae;
	return new OtpErlangTuple(args);
      }
    } else if (t.isInternalAction()) {
      Structure s = (Structure) t;
      return structure2e(s);
    } else if (t.isArithExpr()) {
      System.out.println("Term "+t+" is an arithmetic expression; cannot handle that yet");
      throw new RuntimeException();
    } else if (t.isUnnamedVar()) {
      UnnamedVar u = (UnnamedVar) t;
      return new OtpErlangList(u.getFunctor());
    } else if (t.isCyclicTerm()) {
      System.out.println("Term "+t+" is a cyclic term; cannot handle that yet");
      throw new RuntimeException();
    } else if (t.isList()) {
      ListTerm l = (ListTerm) t;
      List<Term> lts = l.getAsList();
      OtpErlangObject[] arg = new OtpErlangObject[lts.size()];
      for (int i=0; i<lts.size(); i++) {
        arg[i] = t2er(lts.get(i));
      }
      return new OtpErlangList(arg);
    } else if (t.isNumeric()) {
      try {
        NumberTerm n = (NumberTerm) t;
        return new OtpErlangDouble(n.solve());
      } catch (NoValueException exc) {
        throw new RuntimeException();
      }
    } else if (t.isStructure()) {
      Structure s = (Structure) t;
      return structure2e(s);
    } else if (t.isAtom()) {
      Atom a = (Atom) t;
      return new OtpErlangAtom(a.getFunctor());
    } else if (t.isString()) {
      StringTerm s = (StringTerm) t;
      return new OtpErlangList(s.getString());
    } else if (t.isVar()) {
      VarTerm v = (VarTerm) t;
      OtpErlangObject[] args = new OtpErlangObject[2];
      args[0] = new OtpErlangAtom("$var");
      args[1] = new OtpErlangAtom(v.getFunctor());
      return new OtpErlangTuple(args);
    } else {
      System.out.println
        ("Don't know how to translate the term "+t.toString()+
         " to an Erlang term");
      throw new RuntimeException();
    }
  }

  static OtpErlangObject structure2e(Structure s) {
    int arity = s.getArity();
    OtpErlangObject[] arg = new OtpErlangObject[arity+1];
    arg[0] = new OtpErlangAtom(s.getFunctor());
    for (int i=0; i<arity; i++) {
      arg[i+1] = t2er(s.getTerm(i));
    }
    return new OtpErlangTuple(arg);
  }
}
