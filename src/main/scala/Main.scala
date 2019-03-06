import org.opencv.core.Mat

case class Frame(index: Int, matrix: Mat, hist: Option[Mat] = None, accDiff: Double = 0.0, orbResult: Option[Mat] = None)
case class Logo(index: Int, matches: Int)
case class Replay(begin : Int, end: Int)

object Main extends App {
  val filename = "stras_resize.mp4"
  val t = System.currentTimeMillis()
  val shotFrames = ShotDetector(filename, 50000)
  println("Found shots.")
  //LogoDetector(filename, shotFrames)
  
  println("Time: " + (System.currentTimeMillis() - t))
}