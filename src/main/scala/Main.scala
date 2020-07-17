import java.io.File

import cats.effect.{ExitCode, IO, IOApp}
import org.opencv.core.Core

import scala.io.Source
import scala.util.Properties
import cats.implicits._


/**
  * A logo at the beginning or at the end of a replay
  * @param index The frame where the logo is
  * @param matches The frame where the matching logo is
  * @param score The score between the two logos
  * @param tag An optional tag (used when matching against a known logo database, for ex. it can be "liga", "ligue1" etc...)
  */
case class Logo(index: Int, matches: Int, score: Int, tag:Option[String] = None)

case class VideoInfo(startFrame: Int,
                     frameToAnalyse: Int,
                     videoWidth: Int,
                     videoHeight: Int,
                     runId: String)

trait Configuration {
  val logoFolderName          = "frame/"
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

  // def findShot: Vector[Int]
}

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME) // we load the openCV library
    val src = Source.fromFile("to_parse")

    val tasks = src.getLines().toList map {youtubeUrl =>
        println(youtubeUrl)
        YoutubeLogoExtractor(
          youtubeUrl,
          None,
          VideoInfo(
            Properties.envOrElse("startFrame", "0").toInt,
            Properties.envOrElse("frameToAnalyse", "2147483647").toInt,
            Properties.envOrElse("videoWidth", "100").toInt,
            Properties.envOrElse("videoHeight", "100").toInt,
            (System.currentTimeMillis() / 1000L).toString
          )
        )
    }
    tasks.sequence.map(_ => ExitCode.Success)
  }
}

object YoutubeLogoExtractor extends Configuration {

  def apply(youtubeUrl: String,
            knownLogoTag: Option[String],
            videoInfo: VideoInfo)
  : IO[Unit] = {
    IO {
      setupRun(videoInfo.runId)
      println(s"Starting to_parse the video $youtubeUrl.\n$videoInfo")
      downloadFromYoutube(youtubeUrl, videoInfo.runId)
      ReplayDetector(videoInfo.runId + ".mp4", knownLogoTag, videoInfo)
    }.map{_ => println(s"Completed analysis for video $youtubeUrl")}
  }

  def downloadFromYoutube(youtubeUrl: String, runId: String): String = {
    println(s"Downloading ${youtubeUrl} from youtube")
    /*
    val process = Runtime.getRuntime.exec(s"youtube-dl -f 160 $youtubeUrl -o $runId.mp4")
    process.waitFor()
    val error = scala.io.Source.fromInputStream(process.getErrorStream).mkString
    if (error != "") println("ERROR : " + error)
    else println("No error while downloading from youtube")
     */
    import sys.process._
    val cmd = Process(s"youtube-dl -f 160 $youtubeUrl -o $runId.mp4")
    val res = cmd.!
    println(res)
    youtubeUrl
  }

  // ensure every folder exists, also clean the shot folder
  def setupRun(runId: String): Unit = {
    val runFolder = new File(runId)
    runFolder.mkdirs()
    val frameFolder = new File(runId + "/" + logoFolderName)
    val knownLogoFolder = new File(runId + "/" + knownLogoFolderName)
    val shotFolder = new File(runId + "/" + mosaicParentFolderName)
    val shiftMosaicFolder = new File(runId + "/" + shiftedMosaicFolderName)
    val nonShiftedMosaicFolder = new File(runId + "/" + normalMosaicFolderName)
    if (! frameFolder.exists() || ! frameFolder.isDirectory) frameFolder.mkdirs()
    else org.apache.commons.io.FileUtils.cleanDirectory(frameFolder)
    if (! knownLogoFolder.exists() || ! knownLogoFolder.isDirectory) knownLogoFolder.mkdirs()
    else org.apache.commons.io.FileUtils.cleanDirectory(knownLogoFolder)
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
}