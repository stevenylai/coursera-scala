package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization extends VisualizationInterface {

  val EARTH_RADIUS_KM = 6371
  val INVERSE_DIST_P = 2.0

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def dist(l1: Location, l2: Location) = {
      EARTH_RADIUS_KM * {
        if (l1 == l2) {
          0.0
        } else if (l1.lat == -l2.lat && l1.lon == 180 - l2.lon) {
          math.Pi
        } else {
          val t1 = math.sin(math.toRadians(l1.lat)) * math.sin(math.toRadians(l2.lat))
          val t2 = math.cos(math.toRadians(l1.lat)) * math.cos(math.toRadians(l2.lat))
          val t3 = math.cos(math.toRadians(l1.lon) - math.toRadians(l2.lon))
          math.acos(t1 + t2 * t3)
        }
      }
    }
    def weight(dist: Double) = {
      1 / math.pow(dist, INVERSE_DIST_P)
    }
    val distTemp = temperatures.map{ case(l, t) =>
      (dist(l, location), t)
    }
    val close = distTemp.filter(_._1 < 1)
    if (close.nonEmpty) {
      close.head._2
    } else {
      val res = distTemp.foldLeft((0.0, 0.0)){ case((num, denom), (dist, temp)) =>
        (num + temp * weight(dist), denom + weight(dist))
      }
      res._1 / res._2
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toIndexedSeq.sortBy(_._1)
    val upperElems = sortedPoints.dropWhile{ case(t, _) => t < value }
    val lowerElems = sortedPoints.takeWhile{ case(t, _) => t <= value }
    val upperBound = if (upperElems.nonEmpty) upperElems.head else sortedPoints.last
    val lowerBound = if (lowerElems.nonEmpty) lowerElems.last else sortedPoints.head
    if (upperBound == lowerBound) {
      upperBound._2
    } else {
      val delta = upperBound._1 - lowerBound._1
      val upperColor = upperBound._2
      val lowerColor = lowerBound._2
      Color(
        math.round(lowerColor.red + (upperColor.red - lowerColor.red) / delta * (value - lowerBound._1)).toInt,
        math.round(lowerColor.green + (upperColor.green - lowerColor.green) / delta * (value - lowerBound._1)).toInt,
        math.round(lowerColor.blue + (upperColor.blue - lowerColor.blue) / delta * (value - lowerBound._1)).toInt
      )
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val pixelMap = new Array[Pixel](180 * 360)
    for {
      i <- 0 until 180
      j <- 0 until 360
    } {
      val loc = Location(90 - i, j - 180)
      val temp = predictTemperature(temperatures, loc)
      val color = interpolateColor(colors, temp)
      val pix = Pixel(color.red, color.green, color.blue, 127)
      pixelMap(i * 360 + j) = pix
    }
    Image(360, 180, pixelMap)
  }

}

