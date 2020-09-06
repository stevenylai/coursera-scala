package observatory

import java.time.LocalDate

import org.junit.Assert._
import org.junit.Test

trait ExtractionTest extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("data extraction", 1) _

  // Implement tests for the methods of the `Extraction` object
  @Test def `load 2015 example`() = {
    val res = Extraction.locateTemperatures(2015, "/stations-example.csv", "/2015-example.csv")
    println(res)
    println(Extraction.locationYearlyAverageRecords(res))
  }
  @Test def `test avg`() = {
    val data = Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    )
    val expected = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    //assertEquals(expected, Extraction.locationYearlyAverageRecords(data))
  }
}
