package stackoverflow

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import org.junit._
import org.junit.Assert.assertEquals
import java.io.File

object StackOverflowSuite {
  val conf: SparkConf = new SparkConf().setMaster("local").setAppName("StackOverflow")
  val sc: SparkContext = new SparkContext(conf)
}

class StackOverflowSuite {
  import StackOverflowSuite._


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  @Test def `testObject can be instantiated`: Unit = {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  val simplePosts = List(
    Posting(1, 1, None, None, 2, Some("Scala")),  // Question
    Posting(2, 2, None, Some(1), 3, Some("Scala")), // answer 1
    Posting(2, 3, None, Some(1), 4, Some("Scala")), // answer 2
  )
  @Test def `simple join operation`: Unit = {
    val rdd = sc.parallelize(simplePosts)
    val grouped = testObject.groupedPostings(rdd).collect
    assertEquals(1, grouped.size)
    assertEquals(1, grouped(0)._1)
    assertEquals(2, grouped(0)._2.size)
  }

  @Test def `simple scoring`: Unit = {
    val rdd = sc.parallelize(simplePosts)
    val scored = testObject.scoredPostings(testObject.groupedPostings(rdd)).collect
    assertEquals(1, scored.size)
    assertEquals(1, scored(0)._1.id)
    assertEquals(4, scored(0)._2)
  }

  @Test def `simple vector`: Unit = {
    val rdd = sc.parallelize(simplePosts)
    val vectors = testObject.vectorPostings{
      testObject.scoredPostings(testObject.groupedPostings(rdd))
    }.collect
    assertEquals(1, vectors.size)
    assertEquals(10 * testObject.langSpread, vectors(0)._1)
    assertEquals(4, vectors(0)._2)
  }
  @Rule def individualTestTimeout = new org.junit.rules.Timeout(100 * 1000)
}
