package prologP;

import javax.swing.Icon;

public class Tool{
	private int _row,_col;
	private String _clrName;
	private boolean _isQeen;
	public Tool(String color,boolean isQeen,int row,int column) {
		_row =row;
		_col = column;
		_clrName = color;
		_isQeen = isQeen;
	}
	public boolean isQeen() {
		return _isQeen;
	}
	public int getColumn() {
		return _col;
	}
	public int getRow() {
		return _row;
	}
	public String getColorName() {
		return _clrName;
	}
	public boolean equals(Object obj) {
		if(!(obj instanceof Tool)) {
			return false;
		}
		Tool t = (Tool)obj;
		return t.getColumn()==this.getColumn() && this.getRow()==t.getRow() && this._clrName.equals(t.getColorName());
	}
}
