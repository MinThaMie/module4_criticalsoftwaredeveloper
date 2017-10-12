package com.nedap.univeristy.domino;

import java.util.Arrays;


/**
 * This main method solves the three grids for the assignement PDF
 */
public class Main {

    public static void main(String[] args) {
        System.out.println("Grid no.1:");
        Solver solver = new Solver(Arrays.asList(6,6,2,6,5,2,4,1,1,3,2,0,1,0,3,4,1,3,2,4,6,6,5,4,1,0,4,3,2,1,1,2,5,1,3,6,0,4,5,5,5,5,4,0,2,6,0,3,6,0,5,3,4,2,0,3), 6);
        solver.solve();
        System.out.println("Grid no.2:");
        solver = new Solver(Arrays.asList(5,4,3,6,5,3,4,6,0,6,0,1,2,3,1,1,3,2,6,5,0,4,2,0,5,3,6,2,3,2,0,6,4,0,4,1,0,0,4,1,5,2,2,4,4,1,6,5,5,5,3,6,1,2,3,1), 6);
        solver.solve();
        System.out.println("Grid no.3:");
        solver = new Solver(Arrays.asList(4,2,5,2,6,3,5,4,5,0,4,3,1,4,1,1,1,2,3,0,2,2,2,2,1,4,0,1,3,5,6,5,4,0,6,0,3,6,6,5,4,0,1,6,4,0,3,0,6,5,3,6,2,1,5,3), 6);
        solver.solve();
    }
}
