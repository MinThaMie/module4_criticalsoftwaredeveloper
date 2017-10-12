package com.nedap.univeristy.domino;

/**
 * A Stone is a description of two adjacent squares. It describes their values and their position in the array (Field)
 * The first value is always smaller.
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class Stone {
	private int val1;
	private int val2;
	private int pos1;
	private int pos2;

	public Stone(int val1, int val2, int pos1, int pos2) {
		this.val1 = val1;
		this.val2 = val2;
		this.pos1 = pos1;
		this.pos2 = pos2;
	}

	@Override
	public String toString(){
		return "(" + val1 + "," + val2 + ") (" + pos1 + "," + pos2 + ")";
	}


	public int getVal1() {
		return val1;
	}

	public int getVal2() {
		return val2;
	}

	public int getPos1() {
		return pos1;
	}

	public int getPos2() {
		return pos2;
	}
}
