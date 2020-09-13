package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 extends Visualization2Interface {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    val x1 = d00 + point.x * (d10 - d00)
    val x2 = d01 + point.x * (d11 - d01)
    x1 + point.y * (x2 - x1)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    val pixelMap = new Array[Pixel](256 * 256)
    for {
      i <- 0 until 256
      j <- 0 until 256
    } {
      val subtile = Interaction.getSubtileByPixel(i, j, tile, 1, 8)
      val loc = Interaction.tileLocation(subtile)
      val point = CellPoint(loc.lon - loc.lon.floor, loc.lat - loc.lat.floor)
      val d00 = GridLocation(loc.lat.floor.toInt, loc.lon.floor.toInt)
      val d01 = GridLocation(loc.lat.ceil.toInt, loc.lon.floor.toInt)
      val d10 = GridLocation(loc.lat.floor.toInt, loc.lon.ceil.toInt)
      val d11 = GridLocation(loc.lat.ceil.toInt, loc.lon.ceil.toInt)
      val temp = bilinearInterpolation(
        point,
        grid(d00), grid(d01),
        grid(d10), grid(d11)
      )
      val color = Visualization.interpolateColor(colors, temp)
      val pix = Pixel(color.red, color.green, color.blue, 127)
      pixelMap(i + j * 256) = pix
    }
    Image(256, 256, pixelMap)
  }

}
