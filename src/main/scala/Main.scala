import org.bytedeco.javacpp.opencv_videoio.VideoCapture
import org.bytedeco.javacpp.opencv_videoio._
import org.bytedeco.javacpp.opencv_core._


case class Frame(index: Int, matrix: Mat, hist: Option[Mat] = None, accDiff: Double = 0.0, orbResult: Option[Mat] = None)
case class Logo(index: Int, matches: Int, score: Int, tag:Option[String] = None)
case class Replay(begin : Int, end: Int)


trait VideoModule {
  def filename = "video/om.mp4"
  def capture: VideoCapture = new VideoCapture(filename)
  def frameToAnalyse: Int = 10000
  def startFrame: Int = 0
  def videoWidth: Int = 100
  def videoHeight: Int = 100
  // def findShot: Vector[Int]
}

object Main extends App with VideoModule {

  val fps: Double = capture.get(CAP_PROP_FPS)
  val t = System.currentTimeMillis()

  val shotDetector = new ShotDetector with VideoModule
  val replayDetector = new ReplayDetector with VideoModule
  // shot detection
  val shotFrames = shotDetector.findShot //ShotDetector(capture, frameToAnalyse = 10000, startFrame=0, videoHeight = videoHeight, videoWidth = videoWidth)
  val computeSlidingWindowsTime = System.currentTimeMillis() - t
  println("Time to find shots: " + computeSlidingWindowsTime)

  // replay detection
  replayDetector.saveShots(shotFrames.sorted) // !!!! needs to be SORTED (ascending) !!!!
  val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
  println("Time to save shot: " + saveShotTime)

  val logos = replayDetector.matchKnownLogo(shotFrames)
  //val logos = replayDetector.findLogo(shotFrames) // todo : make findLogo private once the testing is finished
  //replayDetector.saveBestLogoShots(logos)
  val computeLogoTime = System.currentTimeMillis() - saveShotTime - t
  println("Time to find logo : " + computeLogoTime)

  val replays = replayDetector.findReplay(logos)

  replayDetector.createPlayList(shotFrames, replays, filename).sortBy(_.index).foreach { r =>
    println(s"Replay (at ${r.index} - ${r.matches}) " +
      s" from ${(r.index/fps/60).toInt}m ${(r.index/fps%60).toInt}s" +
      s" to ${(r.matches/fps/60).toInt}m ${(r.matches/fps%60).toInt}s")
  }

  println("Saving " + logos.length + " logo frames")
  val logoIdx = (logos.map(_.index) ++ logos.map(_.matches)).distinct
  val (videoTag, _) = logos.groupBy(_.tag).mapValues(_.length).max
  OpenCvUtils.saveFrames(capture, logoIdx, "frame/" + videoTag.getOrElse("unk") + "/")

  println("Time total : " + (System.currentTimeMillis() - t))

  capture.release()

}