package com.nedap.univeristy.domino;

import java.util.Objects;

/**
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class Bone {
	private int value;
	private int pip1;
	private int pip2;

	public Bone(int value, int pip1, int pip2) {
		this.value = value;
		this.pip1 = pip1;
		this.pip2 = pip2;
	}

	public int getValue() {
		return this.value;
	}

	public int getPip1() {
		return this.pip1;
	}

	public int getPip2() {
		return this.pip2;
	}

	@Override
	public String toString(){
		return "#" + getValue() + " (" + getPip1() + "," + getPip2() + ")";
	}
}
