import java.io.File
import java.sql.Timestamp

import org.opencv.core.{Core, Mat}
import org.opencv.videoio.VideoCapture
import org.opencv.videoio.Videoio._

import scala.util.Try

/**
  * A logo at the beginning or at the end of a replay
  * @param index The frame where the logo is
  * @param matches The frame where the matching logo is
  * @param score The score between the two logos
  * @param tag An optional tag (used when matching against a known logo database, for ex. it can be "liga", "ligue1" etc...)
  */
case class Logo(index: Int, matches: Int, score: Int, tag:Option[String] = None)

trait Configuration {
  def videoName = "video.mp4" // the video to parse
  def logoTag = None // the known logo tag to use if searching in the known logo database, if none, don't search in the logo DB but only between consecutive shots
  def frameToAnalyse: Int = 2000 // the number of frame to analyze in the video
  def startFrame: Int = 10000 // the frame in the video where the parsing begin (we skip some at the beginning because they are usually noisy)
  def videoWidth: Int = 100 // the width to resize the video to before doing the parsing
  def videoHeight: Int = 100 // the height to resize the video to before doing the parsing
  val frameFolderName         = "frame/"
  val knownLogoFolderName     = "known_logo/"
  val mosaicParentFolderName  = "shot/"
  val shiftedMosaicFolderName = mosaicParentFolderName + "A/"
  val normalMosaicFolderName  = mosaicParentFolderName + "B/"

  // todo : use this wherever possible
  val debugMode: Boolean = false
  /**
    * Mosaic :
    * Before searching for logos, the shots are detected. Because the logos are at the transition between two shots,
    * we only search for logos in the frames between two shots. What we do then is that we search between those
    * transition frames for matching contours.
    * The number of frames we keep between two frames is a variable (mosaicSize); for example, it is of value 10, then
    * we will compare 10 frames at a transition t_s with the 10 frames at the next transition t_s+1 and with the 10
    * frames at the transition t_s+2 for each t_s_i such that 2 < t_s < 90 (because replays lasts between 2 and 90
    * seconds).
    * Therefore, to optimize the comparison, we generate two mosaic images for each transition between two shots.
    * The first one will be a matrix of dim mosaicSize * mosaicSize where each row is the frame of the shots.
    * The second one will be a matrix of dim mosaicSize * mosaicSize where each row is the frame of the shots with an
    * offset of i where i is the row number.
    * Comparing the contours between the two matrics will give us the contours of the two shots in one go.
   */
  def mosaicSize = 20 // the size of the mosaic

  // the date when we parsed the video
  // TODO : make the date clearer ie use DateFormatter or something like that
  def runId:String = new Timestamp(System.currentTimeMillis())
    .toString
    .replaceAll("\\s", "")
    .replaceAll("""([\p{Punct}&&[^.$]]|\b\p{IsLetter}{1,2}\b)\s*""", "_")
    .dropRight(4)
    .replaceAll("\\s", "")
    .trim
  // def findShot: Vector[Int]
}

object Main extends App with Configuration {

  //System.loadLibrary(Core.NATIVE_LIBRARY_NAME) // we load the openCV library
  setup()
  val filename = args.headOption.map(downloadFromYoutube) getOrElse videoName
  val knownLogoTag = Try(args(1)).toOption.orElse(logoTag)

  //replayDetection(filename, knownLogoTag)

  def downloadFromYoutube(youtubeUrl: String) = {
    val process = Runtime.getRuntime().exec("youtube-dl -f 160 " + youtubeUrl + " -o video.mp4")
    process.waitFor()
    youtubeUrl
  }

  // ensure every folder exists, also clean the shot folder
  def setup(): Unit = {
    val frameFolder = new File(frameFolderName)
    val knownLogoFolder = new File(knownLogoFolderName)
    val shotFolder = new File(mosaicParentFolderName)
    val shiftMosaicFolder = new File(shiftedMosaicFolderName)
    val nonShiftedMosaicFolder = new File(normalMosaicFolderName)
    if (! frameFolder.exists() || ! frameFolder.isDirectory) frameFolder.mkdirs()
    if (! knownLogoFolder.exists() || ! knownLogoFolder.isDirectory) knownLogoFolder.mkdirs()
    if (! shotFolder.exists() || ! shotFolder.isDirectory) {
      shotFolder.mkdirs()
      shiftMosaicFolder.mkdirs()
      nonShiftedMosaicFolder.mkdirs()
    } else {
      org.apache.commons.io.FileUtils.cleanDirectory(shotFolder)
      shiftMosaicFolder.mkdirs()
      nonShiftedMosaicFolder.mkdirs()
    }
  }

  /**
    * Starts the replays detection on a video
    * @param filename The filename of the video to parse
    * @param knownLogoTag The logo to match in the logo DB (optional, if not specified, we don't use the logo DB)
    */
  def replayDetection(filename: String, knownLogoTag: Option[String] = None) = {
    knownLogoTag.foreach(t => println("Known logo : " + t))
    val capture: VideoCapture = new VideoCapture(filename)
    val fps: Double = capture.get(CAP_PROP_FPS)
    val t = System.currentTimeMillis()

    val shotDetector = new ShotDetector(capture) with Configuration
    // shot detection
    val shotFrames = shotDetector.findShot
    val foundShots = shotFrames.groupBy(s => (s.toDouble / (fps /2)).round).flatMap(_._2.headOption).toVector
    val computeSlidingWindowsTime = System.currentTimeMillis() - t
    println("Time to find shots: " + computeSlidingWindowsTime)

    // replay detection
    val replayDetector = new ReplayDetector(capture) with Configuration
    replayDetector.saveShots(foundShots.sorted) // !!!! needs to be SORTED (ascending) !!!!
    val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
    println("Time to save shot: " + saveShotTime)


    val logos = knownLogoTag match {
      case Some(tag)=> replayDetector.matchKnownLogo(foundShots, tag)
      case None => replayDetector.findMachingShots(foundShots)
    }

    val computeLogoTime = System.currentTimeMillis() - saveShotTime - t
    println("Time to find logo : " + computeLogoTime)

    val replays = replayDetector.findReplay(logos)

    replayDetector.createPlayList(foundShots, filename, logos = logos)

    println("Saving " + logos.map(_.index).distinct.size + " logo frames")

    logos
      .groupBy(_.tag)
      .foreach{ case (optTag, logosForTag) =>
        OpenCvUtils.saveFrames(capture, logosForTag.map(_.index).distinct, frameFolderName + optTag.getOrElse("unk") + "/", Some(runId + "/"), mosaicSize)
      }

    println("Time total : " + (System.currentTimeMillis() - t))

    capture.release()
  }
}