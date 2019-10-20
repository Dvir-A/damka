package prologP;

import java.util.*;
import org.jpl7.*;
import org.jpl7.Integer;


public class MyQueries {

	public MyQueries(int n,String lvl) throws Exception {
		Query q = new Query("consult",new Atom("C:/Users/dvir0/OneDrive/Documents/Prolog/mmn17.pl"));
 		if(!q.hasMoreElements()) {
			throw new Exception("cant consult prolog");
		}
 		q.close();
		q= new Query("init",new Term[] {new Integer(n),new Atom(lvl)});
		q.hasMoreElements();
		q.close();
	}

	public Term findallTools(String color) throws Exception {
		if(!(color.equals("w") || color .equals("b"))) {
			throw new Exception("wrong color(the color must bu \"w\" or \"b\"");
		}
		Compound tool = new Compound("tool", new Term[] {new Atom(color),new Variable("Type"),new Variable("Row"),new Variable("Col")});
		Query toolsQ = new Query("findall", new Term[] {
				tool,tool,new Variable("ToolsList")
		});
		if(!toolsQ.hasMoreElements()) {
			throw new Exception("no solution found to findall(tool("+color+",Type,Roe,Col).");
		}
		Term res = ((HashMap<String, Term>)toolsQ.nextElement()).get("ToolsList");
		toolsQ.close();
		return res;
	}
	
	/**
	 * check if the tool ,toolBefor , can move to the coordinate in toolAfter and return the eaten to if there is any.
	 * @param toolBefor the origin tool in his origin coordinate.
	 * @param toolAfter the origin tool in the desired coordinate.
	 * @return return the eaten to if there is any , else ,return null.
	 */
	public Term canMove(Compound toolBefor,Compound toolAfter) {
		Query canMoveQ = new Query("canMove", new Term[] {toolBefor,new Variable("Eaten"),toolAfter});
		if(canMoveQ.hasMoreElements()) {
			HashMap<String, Term> map =(HashMap<String, Term>)canMoveQ.nextElement();
			Term res = map.get("Eaten");
			toolAfter.setArg(2, map.get("Type"));
			canMoveQ.close();
			return res;
		}
		canMoveQ.close();
		return null;
	}
	
	public Compound checkPlayerChoise(int fromRow,int fromCol,int toRow,int toCol,Compound pos) {
		Query qPlayerChoise = new Query("check_player_choise", new Term[] {
				new org.jpl7.Integer(fromRow),new org.jpl7.Integer(fromCol),new org.jpl7.Integer(toRow),new org.jpl7.Integer(toCol),
				pos,new Variable("ToPos")
				});
		Map<String, Term> map = qPlayerChoise.oneSolution();
		qPlayerChoise.close();
		if(map == null || map.isEmpty()) {
			return null;
		}
		return (Compound) map.get("ToPos");
	}
	
	public boolean assertTools(Term wTools,Term bTools) {
		this.clean();
		Query qAssertTools = new Query("assertTools", new Term[] {wTools,bTools});
		Boolean bool = qAssertTools.hasMoreElements();
		qAssertTools.close();
		return bool;
	}
	
	public Compound computerTurn(Compound pos) {
		Query compTurnQ = new Query("comp_turn", new Term[] {pos,new Variable("ToPos")});
		Map<String, Term> map = compTurnQ.oneSolution();
		if(map==null || map.isEmpty()) {
			compTurnQ.close();
			return null;
		}
		compTurnQ.close();
		return (Compound) map.get("ToPos");
	}
	
	public Term subDel(Term subList,Term list) {
		Query qSubDel = new Query("sub_del", new Term[] {subList,list,new Variable("Result")});
		if(!qSubDel.hasMoreElements()) {
			return null;
		}
		Term res = ((HashMap<String, Term>)qSubDel.nextElement()).get("Result");
		qSubDel.close();
		return res;
	}
	
	public Term subAdd(Term subList,Term list) {
		Query qSubDel = new Query("sub_del", new Term[] {subList,new Variable("Result"),list});
		if(!qSubDel.hasMoreElements()) {
			return null;
		}
		Term res = ((HashMap<String, Term>)qSubDel.nextElement()).get("Result");
		qSubDel.close();
		return res;
	}
	
	
	public boolean win(Term pos) {
		Query qWin = new Query("win",pos);
		boolean res = qWin.hasMoreElements();
		qWin.close();
		return res;
	}
	
	public void clean() {
		Query qClean = new Query("clean");
		qClean.hasMoreElements();
		qClean.close();
	}
}

