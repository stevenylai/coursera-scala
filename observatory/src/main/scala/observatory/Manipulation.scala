package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation extends ManipulationInterface {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    var saved: Map[GridLocation, Temperature] = Map.empty
    def helper(loc: GridLocation) = {
      if (saved.contains(loc))
        saved(loc)
      else {
        val predicted = Visualization.predictTemperature(temperatures, Location(loc.lat, loc.lon))
        saved = saved + (loc -> predicted)
        predicted
      }
    }
    helper
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val count = temperaturess.size
    val sumFun = temperaturess.map(makeGrid).reduce{
      (f1, f2) => (g: GridLocation) => f1(g) + f2(g)
    }
    (g: GridLocation) => sumFun(g) / count
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val thisGrid = makeGrid(temperatures)
    (g: GridLocation) => {
      thisGrid(g) - normals(g)
    }
  }

}

