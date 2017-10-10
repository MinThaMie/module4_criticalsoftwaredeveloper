package com.nedap.univeristy.domino;

import java.util.List;

/**
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class GameTree {
	Board node;
	GameTree tree;

	public GameTree(Board board, GameTree tree){
		this.node = board;
		this.tree = tree;
	}
}
