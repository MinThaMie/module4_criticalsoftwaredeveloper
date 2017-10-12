package com.nedap.univeristy.domino;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class GameTree {
	private Board node;
	private List<GameTree> children;

	public GameTree(Board board) {
		this.children = new ArrayList<>();
		this.node = board;
		if (!board.isCorrect()) {
			Bone chosen = board.getBones().get(0);
			List<Stone> options = board.findStones(chosen.getPip1(), chosen.getPip2());
			if (!options.isEmpty()) {
				board.removeBone(0);
				for (Stone s : options) {
					Board newBoard = board.clone();
					newBoard.getField().placeStone(s, chosen.getValue());
					newBoard.removeFromOptions(s);
					this.children.add(new GameTree(newBoard));
				}
			}
		}
	}

	public List<Board.Field> findSolution(){
		List<Board.Field> solutions = new ArrayList<>();
		for (GameTree child: children) {
			if (child.node.isFull()) {
				solutions.add(child.node.getField());
			} else {
				solutions.addAll(child.findSolution());
			}
		}
		return solutions;
	}

	private boolean isCorrect(){
		return node.isCorrect();
	}

	public int counter(){
		int count = 0;
		for (GameTree child: children){
			if (child.children.isEmpty()){
				count = count + 1;
			} else {
				count = count + child.counter();
			}
		}
		return count;
	}

	public List<Board.Field> leafs(){
		List<Board.Field> leafs = new ArrayList<>();
		for (GameTree child: children){
			if (child.children.isEmpty()){
				leafs.add(child.node.getField());
			} else {
				leafs.addAll(child.leafs());
			}
		}
		return leafs;
	}
}
