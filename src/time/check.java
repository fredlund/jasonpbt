package time;

import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.*;

import java.util.Calendar;
import java.util.GregorianCalendar;


public class check extends DefaultInternalAction {
  public static Calendar now = null;

  public check() {
    now = new GregorianCalendar();
  }

  @Override
  public int getMinArgs() {
    return 6;
  }

  @Override
  public int getMaxArgs() {
    return 6;
  }

  public static void incSecond() {
    now.add(Calendar.SECOND,1);
  }

  public static void incDay() {
    now.add(Calendar.DAY_OF_MONTH,1);
  }

  @Override
  public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception
  {
    checkArguments(args);

    Object returnValue = 
      un.unifies(args[0], new NumberTermImpl(now.get(Calendar.YEAR)))
      && un.unifies(args[1], new NumberTermImpl(now.get(Calendar.MONTH)))
      && un.unifies(args[2], new NumberTermImpl(now.get(Calendar.DAY_OF_MONTH)))
      && un.unifies(args[3], new NumberTermImpl(now.get(Calendar.HOUR_OF_DAY)))
      && un.unifies(args[4], new NumberTermImpl(now.get(Calendar.MINUTE)))
      && un.unifies(args[5], new NumberTermImpl(now.get(Calendar.SECOND)));

    incSecond();

    return returnValue;
  }
}

