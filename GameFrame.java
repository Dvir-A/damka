package prologP;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.*;

import javax.swing.*;
import org.jpl7.*;

public class GameFrame extends JFrame implements ActionListener{
	private boolean _isBackward=false,_gameEnded=false;
	private ArrayList<Tool> _toolsList;
	private ArrayList<Compound> _prevMovs,_forwardMoves;
	private String _currLvl;
	private MyQueries myQueries;
	private int _size;
	private Compound _currPos;
	private Board _board;
	private final Color darker,lighter;
	private JButton _forwardBtn,_backwardBtn;
	/**
	 * @param n - the dimension of the board
	 * @param lvl - the level of the game.
	 * @throws Exception 
	 */
	public GameFrame(int n) throws Exception {
		super();
		if(n%2 != 0) {
			throw new Exception("Error: The input is not even");  
		}
		_size = n;
		darker = new Color(102, 51, 0);
		lighter = new Color(255, 204, 51);
		_toolsList = new ArrayList<>();
		_prevMovs = new ArrayList<>();
		_forwardBtn = new JButton("forward");
		_forwardBtn.addActionListener(this);
		_backwardBtn = new JButton("backward");
		_backwardBtn.addActionListener(this);
		_forwardMoves = new ArrayList<>();
		this.setLayout(new BorderLayout(10,10));
		_board = new Board(_size);
		add(_board);
		setSize(600, 700);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	//	setResizable(false);
	}

	public void start() throws Exception {
 		String[] lvls = {"easy","normal","hard"};
		_currLvl = (String)JOptionPane.showInputDialog(null, "Please choose a level to play.", "Game levels",JOptionPane.QUESTION_MESSAGE,null, lvls,lvls[1]);
		if(_currLvl==null) {
			JOptionPane.showMessageDialog(null, "terminate : can't play with out a level.");
			System.exit(ABORT);
		}
		try {
			myQueries = new MyQueries(_size, _currLvl);
		} catch (Exception e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(this, "Terminate : can't consult mmn17.pl", "consult error", JOptionPane.ERROR_MESSAGE);
			System.exit(ERROR);
		}
		Term wList = myQueries.findallTools("w");
		Term bList = myQueries.findallTools("b");
		_currPos = new Compound("pos",new Term[] {new Atom("w"),wList,bList});
	//	_prevMovs.add(_currPos);
		setTools();
		repaint();
		play();
	}
	
	public void playerTurn() throws Exception {
		Compound nextPos = null;
		JTextField fromRow,fromCol,toCol,toRow;
		int dialogRes;
		while(nextPos==null) {
			fromRow = new JTextField();
			fromCol = new JTextField();
			toRow = new JTextField();
			toCol = new JTextField();
			Object[] fields = new Object[] {"tool row:",fromRow,"tool column",fromCol,"tool destination row",toRow,"tool destination column:",toCol,_backwardBtn,_forwardBtn};
			dialogRes = JOptionPane.showConfirmDialog(null, fields, "choose the tool position and destinaion", JOptionPane.OK_CANCEL_OPTION);
			if((dialogRes == JOptionPane.CANCEL_OPTION || dialogRes == JOptionPane.CLOSED_OPTION) &&
					(JOptionPane.showConfirmDialog(this, "Do you want to exit?", "", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION))
			{
				System.exit(ABORT);
			}
			try {
				nextPos = myQueries.checkPlayerChoise(java.lang.Integer.parseInt(fromRow.getText()), java.lang.Integer.parseInt(fromCol.getText()), java.lang.Integer.parseInt(toRow.getText()), java.lang.Integer.parseInt(toCol.getText()), _currPos);
			}catch (NumberFormatException e) {
				JOptionPane.showMessageDialog(this, "ERROR:the input should be integer,try again.");		
			}
		}
		if(_isBackward) {
			_forwardMoves.clear();
		}
		_prevMovs.add(_currPos);
		_currPos = nextPos;
		setTools();
		repaint();
	}
	
	public void computerTurn() throws Exception {
 		Term t = myQueries.computerTurn(_currPos);
 		_prevMovs.add(_currPos);
		if(t==null) {
			throw new Exception("eror in computer turn");
		}
		_currPos = (Compound)t;
		setTools();
		repaint();
	}
	 
	public void play() {
		while(!_gameEnded) {
			try {
				playerTurn();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if(myQueries.win(_currPos)) {
				return;
			}
			try {
				computerTurn();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			if(myQueries.win(_currPos)) {
				//tell("computer");
				return;
			}
		}
	}
	
	
	private void setTools() {
		_toolsList.clear();
		Term wList,bList; 
		wList =  _currPos.arg(2);
		bList =  _currPos.arg(3);
		Term[] termArr = wList.toTermArray();
		int row,col;
		for (Term term : termArr) {
			Term rowT = term.arg(3);
			row = rowT.intValue();
			col = term.arg(4).intValue();
			boolean qeened = (term.arg(2).name().equals("qeen")) ? true:false;
			Tool t = new Tool(term.arg(1).name(),qeened,row, col);
			_toolsList.add(t);
		}
		termArr = bList.toTermArray();
		for (Term term : termArr) {
			row = term.arg(3).intValue();
			col = term.arg(4).intValue();
			boolean qeened = (term.arg(2).name().equals("qeen")) ? true:false;
			Tool t = new Tool(term.arg(1).name(),qeened,row, col);
			_toolsList.add(t);
		}
		_board.setToolList(_toolsList);
	}

	
	@Override
	public void actionPerformed(ActionEvent event) {
		String cmd = event.getActionCommand();
		if(cmd.equals("forward")) {
			if(_forwardMoves.isEmpty()) {
				return;
			}else {
				_prevMovs.add(_currPos);
				_currPos = _forwardMoves.remove(_forwardMoves.size()-1);
				setTools();
				repaint();
			}
		}else if(cmd.equals("backward")){
			if(_prevMovs.isEmpty()) {
				return;
			}else {
				_isBackward = true;
				_forwardMoves.add(_currPos);
				_currPos = _prevMovs.remove(_prevMovs.size()-1);
				setTools();
				repaint();
			}
		}
	}
	
	public static void main(String[] args)  {
		GameFrame f;
		try {
			f = new GameFrame(10);
			f.setVisible(true);
			try {
				f.start();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		} catch (Exception e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
	}

	

}
