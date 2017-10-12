package com.nedap.univeristy.domino;

import java.util.Arrays;


/**
 * This main method solves the three grids for the assignement PDF
 */
public class Main {

    public static void main(String[] args) {
//        List<String> input = Arrays.asList( args);
//        List<Integer> field = new ArrayList<>();
//        for(String s : input){
//            field.add(Integer.valueOf(s));
//        }
        Game game = new Game(Arrays.asList(6,6,2,6,5,2,4,1,1,3,2,0,1,0,3,4,1,3,2,4,6,6,5,4,1,0,4,3,2,1,1,2,5,1,3,6,0,4,5,5,5,5,4,0,2,6,0,3,6,0,5,3,4,2,0,3), 6);
        game.play();
    }
}
