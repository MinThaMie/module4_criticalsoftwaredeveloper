package com.nedap.univeristy.domino;

import java.util.*;

/**
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class Game {
	private Board board;

	public Game(List<Integer> field, int maxBone){
		this.board = new Board(field, createBones(maxBone));
	}

	private List<Bone> createBones(int max){
		int no = 1;
		List<Bone> bones = new ArrayList<>();
		for (int pip1 = 0; pip1 <= max; pip1++) {
			for (int pip2 = pip1; pip2 <= max; pip2++){
				Bone bone = new Bone(no, pip1, pip2);
				bones.add(bone);
				no++;
			}
		}
		return bones;
	}

	public Board getBoard() {
		return board;
	}

	public void play(){
		GameTree tree = new GameTree(board);
		List<Board.Field> fields = tree.findSolution();
		if (fields.isEmpty()){
			System.out.println("The following grid has no solutions");
			System.out.println(board.getField());
		} else {
			System.out.println("Original grid: ");
			System.out.println(board.getField());
			System.out.println("The solution(s):");
			int i = 1;
			for (Board.Field field : fields) {
				System.out.println("No. " + i);
				System.out.println(field);
				i++;
			}
		}
	}
}
