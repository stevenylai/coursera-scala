package observatory

import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.log4j.{Level, Logger}

import scala.io.Source
/**
  * 1st milestone: data extraction
  */
object Extraction extends ExtractionInterface {

  @transient lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Observatory")
  @transient lazy val sc: SparkContext = new SparkContext(conf)

  case class StationID(stn: String, wban: String)

  def getRDDFromResource(resource: String) = {
    val fileStream = Source.getClass.getResourceAsStream(resource)
    sc.makeRDD(Source.fromInputStream(fileStream).getLines().toList)
  }
  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
    val stationsLines = getRDDFromResource(stationsFile)
    val yearLines = getRDDFromResource(temperaturesFile)
    val stations = stationsLines.map(line => {
        val arr = line.split(",")
        (
          if (arr.length < 1) None else Some(arr(0)),
          if (arr.length < 2) None else Some(arr(1)),
          if (arr.length >= 4) {
            for {
              lat <- if (arr(2).isEmpty) None else Some(arr(2).toDouble)
              lon <- if (arr(3).isEmpty) None else Some(arr(3).toDouble)
            } yield Location(lat, lon)
          } else None
        )
      }).collect {
        case (Some(stn), Some(wban), Some(l)) => StationID(stn, wban) -> l
    }
    val temperature = yearLines.map(line => {
      val arr = line.split(",")
      (
        if (arr.length < 1) None else Some(arr(0)),
        if (arr.length < 2) None else Some(arr(1)),
        if (arr.length >= 4) {
          for {
            month <- if (arr(2).isEmpty) None else Some(arr(2).toInt)
            day <- if (arr(3).isEmpty) None else Some(arr(3).toInt)
          } yield LocalDate.of(year, month, day)
        } else None,
        if (arr.length < 5 || arr(4).isEmpty) None else Some(arr(4).toDouble)
      )
    }).collect{
      case (Some(stn), Some(wban), Some(d), Some(t)) => (StationID(stn, wban), (d, t))
    }
    stations.join(temperature).values.map{
      case (l, (d, t)) => (d, l, (t - 32) * 5 / 9)
    }.collect().toSeq
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy{
      case (_, l, _) => l
    }.map{ case (k, vs) => {
      val total = vs.map{
        case (_, _, t) => t
      }.sum
      (k, total / vs.size)
    }}
  }
}
