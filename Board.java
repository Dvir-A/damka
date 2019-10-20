package prologP;

import java.awt.*;
import java.util.*;

import javax.swing.*;

public class Board extends JPanel{
	private Rectangle[][] _recMatrix;
	private ArrayList<Tool> _toolList;
	private  int size ;//=600;
	public Board(int n) {
		super();
		size = this.getWidth();
		_recMatrix = new Rectangle[n][n];
		_toolList = new ArrayList<>();
		int step = size/n;
		for (int i = 0; i < _recMatrix.length; i++) {
			for (int j = 0; j < _recMatrix.length; j++) {
				_recMatrix[i][j] = new Rectangle(j*step, i*step, step, step);
			}
		}
	}

	public Dimension getPreferredSize() {
		return new Dimension(600, 600);
	}
	public void setToolList(ArrayList<Tool> tools) {
		if(!tools.isEmpty()) {
			_toolList = new ArrayList<>(tools);
		}
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		size = ((Integer)this.getWidth()).compareTo(this.getHeight()) > 0 ? this.getHeight() : this.getWidth() ;
		int step,cnt=0,length = _recMatrix.length;
		step = size/(length+1);
		int x,y,r= step-10;
		for (int i = 0; i < length; i++) {
			for (int j = 0; j < length; j++) {
				if((cnt%2)==0) {
					g.setColor(new Color(255, 204, 51)); 
				}else {
					g.setColor(new Color(102, 51, 0));
				}
				g.fillRect(j*step, i*step, step, step);
				
				x= j*step+5;
				y= i*step+5;
				if(_toolList.contains(new Tool("w",false,i+1, j+1))) {
					g.setColor(Color.white);
					g.fillOval(x, y, r, r);
				}else if(_toolList.contains(new Tool("b",false,i+1, j+1))) {
					g.setColor(Color.black);
					g.fillOval(x, y, r, r);
				}else if(_toolList.contains(new Tool("w",true,i+1, j+1))) {
					g.setColor(Color.white);
					g.fillOval(x, y, r, r);
					g.setColor(Color.black);
					g.drawString("Q", x+r/2, y+r/2);
				}else if(_toolList.contains(new Tool("b",true,i+1, j+1))) {
					g.setColor(Color.black);
					g.fillOval(x, y, r, r);
					g.setColor(Color.white);
					g.drawString("Q", x+r/2, y+r/2);
				}
				cnt++;
			}
			cnt++;
		}
		x = length*step;
		r = step/2;
		for (int i = 0; i < length; i++) {
			y = i*step;
			g.setColor(Color.BLACK);
			g.drawRect(x, y, step, step);
			g.drawRect(y, x, step, step);
			g.drawString(""+(i+1), x+r, y+r);
			g.drawString(""+(i+1), y+r, x+r);
		}
		//Point p = this.getLocation();
		//Dimension dim = this.getSize();
		
	}

	/*public static void main(String[] args) {
		Board b = new Board(8);
		ArrayList<Point> p = new ArrayList<>();
		int cnt=0;
		for (int i = 0; i < 3; i++) {
			for (int j = 0; j < 8; j++) {
				if(cnt%2==1) {
					p.add(new Point(j, i));
				}
				cnt++;
			}
			cnt++;
		}
		cnt=1;
		for (int i = 5; i < 8; i++) {
			for (int j = 0; j < 8; j++) {
				if(cnt%2==1) {
					p.add(new Point(j, i));
				}
				cnt++;
			}
			cnt++;
		}
		b.setPointsList(p);
		JFrame f = new JFrame();
		f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		f.setSize(600, 600);
		f.setLayout(new BorderLayout());
		f.add(b);
		f.setVisible(true);
	}*/

}
