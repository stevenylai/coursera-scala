package observatory

import org.junit.Test

trait Visualization2Test extends MilestoneSuite {
  private val milestoneTest = namedMilestoneTest("value-added information visualization", 5) _

  // Implement tests for methods of the `Visualization2` object
  val colorMap = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )
}
