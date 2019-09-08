import java.io.File
import java.sql.Timestamp

import YoutubeLogoExtractor.setupRun
import akka.Done
import org.opencv.core.{Core, Mat}
import org.opencv.videoio.VideoCapture
import org.opencv.videoio.Videoio._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.Done
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import com.google.cloud.storage.StorageOptions

import scala.concurrent.{Await, ExecutionContext}
import scala.util.control.NonFatal
// for JSON serialization/deserialization following dependency is required:
// "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.7"
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import scala.concurrent.duration._

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try

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

object WebServer extends App {

  System.loadLibrary(Core.NATIVE_LIBRARY_NAME) // we load the openCV library

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  case class YoutubeRequest(youtubeUrl: String,
                            knownLogoTag: Option[String] = None,  // the, known logo tag to use if searching in the known logo database, if none, don't search in the logo DB but only between consecutive shots
                            frameToAnalyse: Option[Int], // the number of frame to analyze in the video
                            startFrame: Option[Int], // the frame in the video where the parsing begin (we skip some at the beginning because they are usually noisy)
                            videoWidth: Option[Int], // the width to resize the video to before doing the parsing
                            videoHeight: Option[Int]// the height to resize the video to before doing the parsing
                           )

  implicit val itemFormat = jsonFormat6(YoutubeRequest)

  val route: Route =
    concat(
      post {
        path("replay-detector") {
          withRequestTimeout(30.minutes) {
            entity(as[YoutubeRequest]) { youtubeRequest =>
              val saved: Future[Done] =
                YoutubeLogoExtractor(
                  youtubeRequest.youtubeUrl,
                  youtubeRequest.knownLogoTag,
                  videoInfo = VideoInfo(
                    startFrame = youtubeRequest.startFrame.getOrElse(0),
                    frameToAnalyse = youtubeRequest.frameToAnalyse.getOrElse(Int.MaxValue),
                    videoWidth = youtubeRequest.videoWidth.getOrElse(100),
                    videoHeight = youtubeRequest.videoHeight.getOrElse(100),
                    runId = new Timestamp(System.currentTimeMillis())
                      .toString
                      .replaceAll("\\s", "")
                      .replaceAll("""([\p{Punct}&&[^.$]]|\b\p{IsLetter}{1,2}\b)\s*""", "_")
                      .dropRight(4)
                      .replaceAll("\\s", "")
                      .trim
                  )
                ).map { _ => Done }
              onComplete(saved) { done =>
                complete("done parsing the video")
              }
            }
          }
        }
      }
    )


  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 22022)
  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
}

object Main extends App {
  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
  import scala.concurrent.ExecutionContext.Implicits.global
  val res = YoutubeLogoExtractor("", None, VideoInfo(10000, Int.MaxValue, 100, 100, "video"))
  Await.ready(res, Duration.Inf)
}

object YoutubeLogoExtractor extends Configuration {

  def apply(youtubeUrl: String,
            knownLogoTag: Option[String],
            videoInfo: VideoInfo)
           (implicit ec:ExecutionContext)
  : Future[Unit] = {
    Future {
      setupRun(videoInfo.runId)
      println(s"Starting to parse the video $youtubeUrl.\n$videoInfo")
      //downloadFromYoutube(youtubeUrl, videoInfo.runId)
      replayDetection(videoInfo.runId + ".mp4", knownLogoTag, videoInfo)
    }//.flatMap(_ => uploadToGCP(videoInfo.runId + "/" + logoFolderName, "gs://logo_detection_shots"))
      .map{_ => println(s"Completed analysis for video $youtubeUrl")}
      .recover {
        case NonFatal(ex) =>
          ex.printStackTrace()
      }
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

  def uploadToGCP(folderPath: String,
                  bucket: String)
                 (implicit ec: ExecutionContext)
  : Future[Unit] = Future {
    println(s"Uploading $folderPath to GCP")
    import sys.process._
    val cmd = Process("gsutil -m cp -r " + folderPath + " " + bucket)
    val res = cmd.!
    println(res)
    /*
    val process = Runtime.getRuntime.exec("gsutil -m cp -r " + folderPath + " " + bucket)
    process.waitFor()
    val error = scala.io.Source.fromInputStream(process.getErrorStream).mkString
    if (error != "") println("ERROR : " + error)
    else println("No error while uploading to GCP")
     */
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

  /**
    * Starts the replays detection on a video
    * @param filename The filename of the video to parse
    * @param knownLogoTag The logo to match in the logo DB (optional, if not specified, we don't use the logo DB)
    */
  def replayDetection(filename: String,
                      knownLogoTag: Option[String] = None,
                      videoInfo: VideoInfo
                     ) = {
    knownLogoTag.foreach(t => println("Known logo : " + t))
    val capture: VideoCapture = new VideoCapture(filename)
    val fps: Double = capture.get(CAP_PROP_FPS)
    val t = System.currentTimeMillis()

    val shotDetector = new ShotDetector(capture, videoInfo) with Configuration
    // shot detection
    val shotFrames = shotDetector.findShot
    val foundShots = shotFrames.groupBy(s => (s.toDouble / (fps /2)).round).flatMap(_._2.headOption).toVector.dropRight(1) // fixme: dropRight(1) is a hack, otherwise that loop never ends (test case youtube url : 1rWw2LkYzAQ)
    val computeSlidingWindowsTime = System.currentTimeMillis() - t
    println(s"Time to find shots (found ${foundShots.length}): $computeSlidingWindowsTime")

    // replay detection
    /*
    val replayDetector = new ReplayDetector(capture, videoInfo) with Configuration
    replayDetector.saveShots(
      videoInfo.runId,
      foundShots.sorted,
      videoInfo.runId + "/" + mosaicParentFolderName, // !!!! needs to be SORTED (ascending) !!!!
      videoInfo.runId + "/" + logoFolderName) // !!!! needs to be SORTED (ascending) !!!!
    val saveShotTime = System.currentTimeMillis() - t - computeSlidingWindowsTime
    println("Time to save shot: " + saveShotTime)
*/
/*
    val logos = knownLogoTag match {
      case Some(tag)=> replayDetector.matchKnownLogo(videoInfo.runId, foundShots, tag)
      case None => replayDetector.findMachingShots(videoInfo.runId, foundShots)
    }

    val computeLogoTime = System.currentTimeMillis() - saveShotTime - t
    println("Time to find logo : " + computeLogoTime)

    val replays = replayDetector.findReplay(logos)

    //replayDetector.createPlayList(foundShots, filename, logos = logos)

    println("Saving " + logos.map(_.index).distinct.size + " logo frames")

    logos
      .groupBy(_.tag)
      .foreach{ case (optTag, logosForTag) =>
        OpenCvUtils.saveFrames(capture, logosForTag.map(_.index).distinct, videoInfo.runId + "/" + logoFolderName + optTag.getOrElse("unk") + "/", videoInfo.runId, mosaicSize)
      }

    // save non logo shots
    val distinctLogos = (logos.map(_.index) ++ logos.map(_.matches)).toSet
    val nonLogos = scala.util.Random
      .shuffle(shotFrames.toSet -- distinctLogos)
      .take(distinctLogos.size)
  */
    OpenCvUtils.saveFrames(capture, foundShots, videoInfo.runId + "/" + logoFolderName + "not_logo", videoInfo.runId, mosaicSize)

    println("Time total : " + (System.currentTimeMillis() - t))

    capture.release()
  }
}