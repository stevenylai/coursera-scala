package observatory

import scala.collection.concurrent.TrieMap
import org.junit.Assert._
import org.junit.Test

trait InteractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("interactive visualization", 3) _

  def getSubtile(tile: Tile): List[Tile] = {
    List(
      Tile(tile.x * 2, tile.y * 2, tile.zoom + 1),
      Tile(tile.x * 2 + 1, tile.y * 2, tile.zoom + 1),
      Tile(tile.x * 2, tile.y * 2 + 1, tile.zoom + 1),
      Tile(tile.x * 2 + 1, tile.y * 2 + 1, tile.zoom + 1),
    )
  }
  def getSubtiles(tile: Tile, depth: Int): List[Tile] = {
    if (depth == 0) List(tile)
    else {
      getSubtile(tile).flatMap(t => getSubtiles(t, depth - 1))
    }
  }
  def getTilesByPixels(tile: Tile, depth: Int) = {
    for {
      i <- 0 until math.pow(2, depth).toInt
      j <- 0 until math.pow(2, depth).toInt
    } yield Interaction.getSubtileByPixel(i, j, tile, 1, depth)
  }
  @Test def `get subtile shoud get the same tile set`() = {
    val root = Tile(0, 0, 0)
    assertEquals(
      getSubtiles(root, 8).toSet,
      getTilesByPixels(root, 8).toSet
    )
  }
  @Test def `get subtile`() = {
    val tile = Tile(1, 1, 1)
    val subtile1 = Interaction.getSubtileByPixel(0, 1, tile, 1, 1)
    assertEquals(subtile1, Tile(2, 3, 2))
    assertEquals(
      Interaction.getSubtileByPixel(1, 0, subtile1, 1, 1),
      Interaction.getSubtileByPixel(1, 2, tile, 1, 2)
    )
  }
}
