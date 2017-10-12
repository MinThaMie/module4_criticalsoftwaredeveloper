package test;

import com.nedap.univeristy.domino.Board;
import com.nedap.univeristy.domino.GameTree;
import com.nedap.univeristy.domino.Stone;
import org.junit.Before;
import org.junit.Test;
import com.nedap.univeristy.domino.Game;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Created by anne-greeth.vanherwijnen on 10/10/2017.
 */
public class TestGame {
	private Game gameSix;
	private Board gameSixBoard;
	private Game secondGame;
	private Game flawedGame;

	@Before
	public void setUp() {
		gameSix = new Game(Arrays.asList(6,6,2,6,5,2,4,1,1,3,2,0,1,0,3,4,1,3,2,4,6,6,5,4,1,0,4,3,2,1,1,2,5,1,3,6,0,4,5,5,5,5,4,0,2,6,0,3,6,0,5,3,4,2,0,3), 6);
		gameSixBoard = gameSix.getBoard();
		flawedGame = new Game(Arrays.asList(5,6,2,6,5,2,4,1,1,3,2,0,1,0,3,4,1,3,2,4,6,6,5,4,1,0,4,3,2,1,1,2,5,1,3,6,0,4,5,5,5,5,4,0,2,6,0,3,6,0,5,3,4,2,0,3), 6);
		secondGame = new Game(Arrays.asList(4,2,5,2,6,3,5,4,5,0,4,3,1,4,1,1,1,2,3,0,2,2,2,2,1,4,0,1,3,5,6,5,4,0,6,0,3,6,6,5,4,0,1,6,4,0,3,0,6,5,3,6,2,1,5,3), 6);
	}
	@Test
	public void createBonesTest() {
		assertEquals(28, gameSixBoard.getBones().size());
	}

	@Test
	public void allOptionsTest() {
		assertEquals(97, gameSixBoard.getOptions().size());
	}

	@Test
	public void findBoneValueTest(){
		assertEquals(28, gameSixBoard.findBoneValue(6,6));
		assertEquals(-1 , gameSixBoard.findBoneValue(10,10));
	}

	@Test
	public void findStoneTest(){
		assertEquals(1, gameSixBoard.findStones(4,4).size());
		assertEquals(3, gameSixBoard.findStones(5,5).size());
	}

	@Test
	public void removeOptionsTest(){
		List<Stone> optionsBeforeRemove = gameSixBoard.getOptions();
		gameSixBoard.removeFromOptions(gameSixBoard.findStones(4,4).get(0));
		List<Stone> optionsAfterRemove = gameSixBoard.getOptions();
		assertEquals(92, optionsAfterRemove.size()); // Minius the stone  and minus all the effected other options
	}

	@Test
	public void testGameTree(){
		GameTree gameTreeGameSix = new GameTree(gameSixBoard);
		List<Board.Field> fields = gameTreeGameSix.findSolution();
		GameTree secondGameTree = new GameTree(secondGame.getBoard());
		List<Board.Field> fields2 = secondGameTree.findSolution();
		GameTree flawedGameTree = new GameTree(flawedGame.getBoard());
		List<Board.Field> noFields = flawedGameTree.findSolution();
		for (Board.Field field : fields2){
			System.out.println(field);
		}
		assertEquals(4, fields.size());
		assertEquals(2, fields2.size());
		assertTrue(noFields.isEmpty());
	}

	@Test
	public void testCounter(){
		GameTree gameTreeGameSix = new GameTree(gameSixBoard);
		assertEquals(3763, gameTreeGameSix.leafs().size());
	}

}
