package com.nedap.univeristy.domino;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {

    public static void main(String[] args) {
        List<String> input = Arrays.asList( args);
        List<Integer> field = new ArrayList<>();
        for(String s : input){
            field.add(Integer.valueOf(s));
        }
        Game game = new Game(field, 6);
        System.out.println(game.getBoard().getField());
    }
}
