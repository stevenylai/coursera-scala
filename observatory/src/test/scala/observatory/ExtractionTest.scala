package observatory

import org.apache.log4j.{Level, Logger}
import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  @Test def `load 2015 example`: Unit = {
    val res = Extraction.locateTemperatures(2015, "/stations-example.csv", "/2015-example.csv")
    println(res)
    println(Extraction.locationYearlyAverageRecords(res))
  }
}
