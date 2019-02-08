import org.opencv.core.Mat

case class Frame(index: Int, matrix: Mat, hist: Option[Mat] = None, accDiff: Double = 0.0)
case class Replay(begin : Int, end: Int)

object Main extends App {
  
  val filename = "Week 15 - Pittsburgh Steelers  vs New England Patriots-rHBAACr5nio.mkv"
  val t = System.currentTimeMillis()
  ShotDetector(filename, 5000, startFrame = 20000, display = true)
  //LogoDetector(filename, 10000)
  println("Time: " + (System.currentTimeMillis() - t))
}

