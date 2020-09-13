package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec

/**
  * 3rd milestone: interactive visualization
  */
object Interaction extends InteractionInterface {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    Location(
      math.toDegrees(math.atan(math.sinh(math.Pi * (1.0 - 2.0 * tile.y / (1 << tile.zoom))))),
      tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0,
    )
  }

  @tailrec
  def getSubtileByPixel(pixelX: Int, pixelY: Int, tile: Tile,
                        curDepth: Int, maxDepth: Int): Tile = {
    assert(pixelX >= 0 && pixelX < math.pow(2, maxDepth).toInt)
    assert(pixelY >= 0 && pixelY < math.pow(2, maxDepth).toInt)
    if (curDepth > maxDepth) {
      tile
    } else {
      val halfPixels = math.pow(2, maxDepth - curDepth).toInt
      getSubtileByPixel(
        if (pixelX < halfPixels) pixelX else pixelX - halfPixels,
        if (pixelY < halfPixels ) pixelY else pixelY - halfPixels,
        Tile(
          if (pixelX < halfPixels) 2 * tile.x else 2 * tile.x + 1,
          if (pixelY < halfPixels) 2 * tile.y else 2 * tile.y + 1,
          tile.zoom + 1
        ),
        curDepth + 1, maxDepth)
    }
  }
  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    val pixelMap = new Array[Pixel](256 * 256)
    for {
      i <- 0 until 256
      j <- 0 until 256
    } {
      val subtile = getSubtileByPixel(i, j, tile, 1, 8)
      val temp = Visualization.predictTemperature(temperatures, tileLocation(subtile))
      val color = Visualization.interpolateColor(colors, temp)
      val pix = Pixel(color.red, color.green, color.blue, 127)
      pixelMap(i + j * 256) = pix
    }
    Image(256, 256, pixelMap)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      data <- yearlyData
      zoom <- 0 to 3
      x <- 0 until math.pow(2, zoom).toInt
      y <- 0 until math.pow(2, zoom).toInt
    } generateImage(data._1, Tile(x, y, zoom), data._2)
  }

}
