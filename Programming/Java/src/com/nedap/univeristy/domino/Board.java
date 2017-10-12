package com.nedap.univeristy.domino;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * A board contains a grid (might be partially filled), the options that you can still place and the bones that need to be placed.
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class Board {
	private Field field;
	private List<Stone> options;
	private List<Bone> bones;
	private boolean full;

	public Board(List<Integer> field, List<Bone> bones){
		this.field = new Field(field);
		this.options = createOptions();
		this.bones = bones;
		this.full = false;
	}

	public Board(Field field, List<Stone> options, List<Bone> bones) {
		this.field = field;
		this.options = options;
		this.bones = bones;
		this.full = bones.isEmpty();
	}

	public boolean isFull() {
		return full;
	}

	public Board cloneBoard(){
		return new Board(field.cloneField(), cloneOptions(),cloneBones());
	}

	public int findBoneValue(int x, int y){
		for (Bone bone : bones){
			if (bone.getPip1() == x && bone.getPip2() == y){
				return bone.getValue();
			}
		}
		return -1;
	}

	public List<Stone> findStones(int x, int y){ // X is always smaller
		List<Stone> stones = new ArrayList<>();
		for (Stone stone : options){
			if (stone.getVal1() == x && stone.getVal2() == y){
				stones.add(stone);
			}
		}
		return stones;
	}

	public List<Stone> findPosition(int x, int y){
		List<Stone> stones = new ArrayList<>();
		for (Stone stone: options){
			if (stone.getPos1() == x || stone.getPos2() == x || stone.getPos1() == y || stone.getPos2() == y){
				stones.add(stone);
			}
		}
		return stones;
	}

	public List<Bone> getBones(){
		return this.bones;
	}

	public void removeBone(int index){
		bones.remove(index);
	}

	public List<Bone> cloneBones() {
		List<Bone> deepcopy = new ArrayList<>();
		for (Bone b : bones){
			deepcopy.add(b);
		}
		return deepcopy;
	}

	/**
	 * This function creates all the options for the bones on the board, horizontally and vertically.
	 * The values are ordered to match with the bones.
	 */
	private List<Stone> createOptions(){
		List<Stone> options = new LinkedList<>(); // TODO: Maybe arraylist
		for (int snd = 1; snd < field.getSize(); snd++){ // Horizontal options
			if (snd % 8 != 0) {
				int valx = field.getElem(snd - 1);
				int valy =  field.getElem(snd);
				int posx = snd - 1;
				int posy = snd;
				if (valx <= valy) {
					Stone stone = new Stone(valx, valy, posx, posy);
					options.add(stone);
				} else {
					Stone stone = new Stone(valy, valx, posy, posx);
					options.add(stone);
				}
			}
		}
		for (int fst = 0; fst < field.getSize() - 8; fst++){ // Vertical options
			int valx = field.getElem(fst);
			int valy =  field.getElem(fst + 8);
			int posx = fst;
			int posy = fst + 8;
			if (valx <= valy) {
				Stone stone = new Stone(valx, valy, posx, posy);
				options.add(stone);
			} else {
				Stone stone = new Stone(valy, valx, posy, posx);
				options.add(stone);
			}
		}
		return  options;
	}

	private List<Stone> cloneOptions(){
		List<Stone> deepcopy = new ArrayList<>();
		for (Stone s : options){
			deepcopy.add(s);
		}
		return deepcopy;
	}

	public void removeFromOptions(Stone stone){
		options.removeAll(findStones(stone.getVal1(),stone.getVal2()));
		options.removeAll(findPosition(stone.getPos1(),stone.getPos2()));
	}

	public Field getField() {
		return field;
	}

	public List<Stone> getOptions() {
		return options;
	}

	public class Field {
		public List<Integer> field;
		public Field (List<Integer> field){
			this.field = field;
		}

		public void placeStone(Stone stone, int bonevalue){
			field.set(stone.getPos1(), bonevalue);
			field.set(stone.getPos2(), bonevalue);
		}

		public Integer getElem(int i){
			return field.get(i);
		}

		public int getSize(){
			return field.size();
		}

		public Field cloneField(){
			List<Integer> deepcopy = new ArrayList<>();
			for (Integer i : field){
				deepcopy.add(i);
			}
			return new Field(deepcopy);
		}

		@Override
		public String toString(){
			String grid = "";
			for (int i = 0; i < field.size(); i++){
				String elem = "";
				grid = grid + " " + ( field.get(i) < 10 ? " " + field.get(i) + " " : "" + field.get(i) + " " ) + (i % 8 == 7 ? "\n" : "");
			}
			return grid;
		}

	}
}
