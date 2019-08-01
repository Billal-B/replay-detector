import java.io.File
import java.sql.Timestamp

import org.opencv.core.{Core, Mat}
import org.opencv.videoio.VideoCapture
import org.opencv.videoio.Videoio._


case class Frame(index: Int, matrix: Mat, hist: Option[Mat] = None, accDiff: Double = 0.0, orbResult: Option[Mat] = None)
case class Logo(index: Int, matches: Int, score: Int, tag:Option[String] = None)
case class Replay(begin : Int, end: Int)


trait Configuration {
  def videoName = "video.mp4"
  def frameToAnalyse: Int = Int.MaxValue
  def startFrame: Int = 0
  def videoWidth: Int = 100
  def videoHeight: Int = 100
  def knownLogo: Boolean = true // todo : move this (conf module)
  def mosaicSize = 1 // mosaic is a matrix of size mosaicWidth * mosaicWidth
  def numberOfMosaic = 10
  def saveWindowSize2 = 10
  def uploadToS3 = false

  def runId:String = new Timestamp(System.currentTimeMillis())
    .toString
    .replaceAll("\\s", "")
    .replaceAll("""([\p{Punct}&&[^.$]]|\b\p{IsLetter}{1,2}\b)\s*""", "")
    .dropRight(4)
    .replaceAll("\\s", "")
    .trim
  // def findShot: Vector[Int]
}

object Main extends App with Configuration {

  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
  setup()
  val filename = args.headOption getOrElse videoName
  replayDetection(filename)
  if (uploadToS3) {
    uploadLogosToS3()
    uploadKnownLogoToS3()
  }

  // ensure every folder exists, also clean the shot folder
  def setup(): Unit = {
    val frameFolder = new File("frame")
    val knownLogoFolder = new File("known_logo")
    val shotFolder = new File("shot")
    val shotAFolder = new File("shot/A")
    val shotBFolder = new File("shot/B")
    if (! frameFolder.exists() || ! frameFolder.isDirectory) frameFolder.mkdirs()
    if (! knownLogoFolder.exists() || ! knownLogoFolder.isDirectory) knownLogoFolder.mkdirs()
    if (! shotFolder.exists() || ! shotFolder.isDirectory) {
      shotFolder.mkdirs()
      shotAFolder.mkdirs()
      shotBFolder.mkdirs()
    } else {
      org.apache.commons.io.FileUtils.cleanDirectory(shotFolder)
      shotAFolder.mkdirs()
      shotBFolder.mkdirs()
    }
  }

  // reads the logos in the frame directory and uploads them to s3 in the correct bucket and directory
  def uploadLogosToS3() = {
    val logoFolder = new File("./frame").listFiles().map{tag =>
      val imgs = new File(tag.getCanonicalPath).listFiles().map(_.getAbsolutePath)
      AWSUtils.uploadToS3(imgs, "bb-replay-detector-logos", tag.getName)
    }
  }

  def uploadKnownLogoToS3() = {
    val logoFolder = new File("./known_logo").listFiles().map{tag =>
      val imgs = new File(tag.getCanonicalPath).listFiles().map(_.getAbsolutePath)
      AWSUtils.uploadToS3(imgs, "bb-replay-detector-known-logos", tag.getName)
    }
  }

  def replayDetection(filename: String) = {
    val capture: VideoCapture = new VideoCapture(filename)
    val fps: Double = capture.get(CAP_PROP_FPS)
    val t = System.currentTimeMillis()

    val shotDetector = new ShotDetector(capture) with Configuration
    // shot detection
    val shotFrames = shotDetector.findShot
    val computeSlidingWindowsTime = System.currentTimeMillis() - t
    println("Time to find shots: " + computeSlidingWindowsTime)

    // replay detection
    val replayDetector = new ReplayDetector(capture) with Configuration
    val foundShots = shotFrames.groupBy(s => (s.toDouble / (fps /2)).round).flatMap(_._2.headOption).toVector
    replayDetector.saveShots(foundShots.sorted) // !!!! needs to be SORTED (ascending) !!!!
    val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
    println("Time to save shot: " + saveShotTime)

    val logos = if (knownLogo) replayDetector.matchKnownLogo(foundShots) else {
      val _logos = replayDetector.findLogo(foundShots)
      _logos
    }
    val computeLogoTime = System.currentTimeMillis() - saveShotTime - t
    println("Time to find logo : " + computeLogoTime)

    val replays = replayDetector.findReplay(logos)

    replayDetector.createPlayList(foundShots, replays, filename).sortBy(_.index).foreach { r =>
      println(s"Replay (at ${r.index} - ${r.matches}) " +
        s" from ${(r.index/fps/60).toInt}m ${(r.index/fps%60).toInt}s" +
        s" to ${(r.matches/fps/60).toInt}m ${(r.matches/fps%60).toInt}s")
    }

    println("Saving " + logos.length + " logo frames")

    logos
      .groupBy(_.tag)
      .foreach{case (optTag, logos) =>
        val logosToSave =
          if (knownLogo) logos.map(_.index)
          else logos.flatMap(l => Vector(l.index, l.matches))
        OpenCvUtils.saveFrames(capture, logosToSave, "frame/" + optTag.getOrElse("unk") + "/", Some(runId + "/"), saveWindowSize2)
      }

    println("Time total : " + (System.currentTimeMillis() - t))

    capture.release()
  }



}