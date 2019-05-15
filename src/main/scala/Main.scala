import java.io.File
import java.nio.file.{Files, Paths}

import com.amazonaws.services.s3.{AmazonS3Client, AmazonS3ClientBuilder}
import org.bytedeco.javacpp.opencv_videoio.VideoCapture
import org.bytedeco.javacpp.opencv_videoio._
import org.bytedeco.javacpp.opencv_core._

import scala.util.Try


case class Frame(index: Int, matrix: Mat, hist: Option[Mat] = None, accDiff: Double = 0.0, orbResult: Option[Mat] = None)
case class Logo(index: Int, matches: Int, score: Int, tag:Option[String] = None)
case class Replay(begin : Int, end: Int)


trait VideoModule {
  def filename = "video/Full Match FC Barcelona vs Valencia CF LaLiga 2017_2018-h1OJFu9dmJA.mkv"
  def frameToAnalyse: Int = 10000
  def startFrame: Int = 0
  def videoWidth: Int = 100
  def videoHeight: Int = 100
  // def findShot: Vector[Int]
}

object Main extends App with VideoModule {

  val uploadToS3 = false
  val knownLogo = false

  replayDetection()
  if (uploadToS3) uploadLogosToS3()

  // ensure every folder exists, also clean the shot folder
  def setup(): Unit = {
    val frameFolder = new File("frame")
    val knownLogoFolder = new File("known_logo")
    val shotFolder = new File("shot")
    if (! frameFolder.exists() || ! frameFolder.isDirectory) frameFolder.mkdirs()
    if (! knownLogoFolder.exists() || ! knownLogoFolder.isDirectory) knownLogoFolder.mkdirs()
    if (! shotFolder.exists() || ! shotFolder.isDirectory) {
      shotFolder.mkdirs()
    } else {
      shotFolder.delete()
      shotFolder.mkdir()
    }
  }

  // reads the logos in the frame directory and uploads them to s3 in the correct bucket and directory
  def uploadLogosToS3() = {
    val logoFolder = new File("./frame").listFiles().map{tag =>
      val imgs = new File(tag.getCanonicalPath).listFiles().map(_.getAbsolutePath)
      AWSUtils.uploadToS3(imgs, "bb-replay-detector-logos", tag.getName)
    }
  }

  def replayDetection() = {
    val capture: VideoCapture = new VideoCapture(filename)
    val fps: Double = capture.get(CAP_PROP_FPS)
    val t = System.currentTimeMillis()

    val shotDetector = new ShotDetector(capture) with VideoModule
    // shot detection
    val shotFrames = shotDetector.findShot
    val computeSlidingWindowsTime = System.currentTimeMillis() - t
    println("Time to find shots: " + computeSlidingWindowsTime)

    // replay detection
    val replayDetector = new ReplayDetector(capture) with VideoModule
    val foundShots = shotFrames.groupBy(s => (s.toDouble / (fps /2)).round).flatMap(_._2.headOption).toVector
    replayDetector.saveShots(foundShots.sorted) // !!!! needs to be SORTED (ascending) !!!!
    val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
    println("Time to save shot: " + saveShotTime)

    val logos = if (knownLogo) replayDetector.matchKnownLogo(foundShots) else {
      val _logos = replayDetector.findLogo(foundShots)
      replayDetector.saveBestLogoShots(_logos)
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
    val logoIdx = (logos.map(_.index) ++ logos.map(_.matches)).distinct
    val videoTag = Try{logos.groupBy(_.tag).mapValues(_.length).max}.toOption.flatMap(_._1)
    OpenCvUtils.saveFrames(capture, logoIdx, "frame/" + videoTag.getOrElse("unk") + "/")

    println("Time total : " + (System.currentTimeMillis() - t))

    capture.release()
  }



}