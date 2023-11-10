package games.gameOfFifteen

import board.Cell
import board.SquareBoard
import board.Direction
import board.createGameBoard
import games.game.Game

/*
 * Implement the Game of Fifteen (https://en.wikipedia.org/wiki/15_puzzle).
 * When you finish, you can play the game by executing 'PlayGameOfFifteen'.
 */
fun newGameOfFifteen(initializer: GameOfFifteenInitializer = RandomGameInitializer()): Game =
    GameOfFifteen(initializer)

class GameOfFifteen(private val initializer: GameOfFifteenInitializer): Game {
    private val board = createGameBoard<Int?>(4)

    override fun initialize() {
        val tiles = initializer.initialPermutation
        board.run{
            (1..4)
                .flatMap{i ->
                    (1..4)
                        .map{j -> i to j}
                }
                .zip(tiles)
                .forEach{pair ->
                    val coord = pair.first
                    val tile = pair.second
                    set(getCell(coord.first, coord.second), tile)
                }
        }
    }

    override fun canMove() = true

    override fun hasWon() = board
            .getAllCells()
            .asSequence()
            .sortedBy { it.i * board.width + it.j }
            .map{cell -> board.get(cell)}
            .toList() == (1..15).toList().plusElement(null)

    override fun processMove(direction: Direction) {
        with(board) {
            val target = find { it  == null }!!
            target
                .getNeighbour(when(direction){
                    Direction.UP -> Direction.DOWN
                    Direction.DOWN -> Direction.UP
                    Direction.RIGHT -> Direction.LEFT
                    Direction.LEFT -> Direction.RIGHT
                })
                ?.also { s ->
                    val tile = get(s)
                    set(s, null)
                    set(target, tile)
                }
        }
    }

    override fun get(i: Int, j: Int): Int? = board.run { get(getCell(i, j)) }
}
