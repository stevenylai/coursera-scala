package board

import board.Direction.*
import java.lang.IllegalArgumentException

fun createSquareBoard(width: Int): SquareBoard = object : SquareBoard {
    val cells = IntRange(1, width)
        .map {i -> IntRange(1, width)
            .map{j -> Cell(i, j)}
        }

    override val width: Int
        get() = width

    override fun getCellOrNull(i: Int, j: Int): Cell? {
        return if (i >= 1 && i <= width && j >= 1 && j <= width)
            cells[i - 1][j - 1]
        else
            null
    }

    override fun getCell(i: Int, j: Int): Cell {
        val cell = getCellOrNull(i, j)
        if (cell == null) {
            throw IllegalArgumentException()
        } else{
            return cell
        }
    }

    override fun getAllCells(): Collection<Cell> {
        return cells.flatten()
    }

    override fun getRow(i: Int, jRange: IntProgression): List<Cell> {
        return jRange
            .map{j -> i to j}
            .filter{pair -> pair.first in 1..width &&
                    pair.second in 1 .. width
            }
            .map{pair -> getCell(pair.first, pair.second)}
    }

    override fun getColumn(iRange: IntProgression, j: Int): List<Cell> {
        return iRange
            .map{i -> i to j}
            .filter{pair -> pair.first in 1..width &&
                    pair.second in 1 .. width
            }
            .map{pair -> getCell(pair.first, pair.second)}
    }

    override fun Cell.getNeighbour(direction: Direction): Cell? {
        return when (direction) {
            UP -> getCellOrNull(i - 1, j)
            DOWN -> getCellOrNull(i + 1, j)
            RIGHT -> getCellOrNull(i, j + 1)
            LEFT -> getCellOrNull(i, j - 1)
        }
    }
}
fun <T> createGameBoard(width: Int): GameBoard<T> = object : SquareBoard by createSquareBoard(width), GameBoard<T> {
    val values = HashMap<Cell, T>()
    override fun get(cell: Cell): T? {
        return values.get(cell)
    }

    override fun all(predicate: (T?) -> Boolean): Boolean {
        return getAllCells()
            .asSequence()
            .map{c -> get(c)}
            .all(predicate)
    }

    override fun any(predicate: (T?) -> Boolean): Boolean {
        return getAllCells()
            .asSequence()
            .map{c -> get(c)}
            .any(predicate)
    }

    override fun find(predicate: (T?) -> Boolean): Cell? {
        return getAllCells()
            .asSequence()
            .map{c -> c to get(c)}
            .filter{pair -> predicate(pair.second)}
            .map{it.first}
            .firstOrNull()
    }

    override fun filter(predicate: (T?) -> Boolean): Collection<Cell> {
        return getAllCells()
            .asSequence()
            .map{c -> c to get(c)}
            .filter{pair -> predicate(pair.second)}
            .map{it.first}
            .toList()
    }

    override fun set(cell: Cell, value: T?) {
        if (value != null)
            values[cell] = value
        else
            values.remove(cell)
    }

}

