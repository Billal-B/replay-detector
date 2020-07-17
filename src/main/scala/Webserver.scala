import java.sql.Timestamp

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.{as, complete, concat, entity, onComplete, path, post, withRequestTimeout}
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import spray.json.DefaultJsonProtocol.jsonFormat6
// for JSON serialization/deserialization following dependency is required:
// "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.7"
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import scala.concurrent.duration._
import scala.concurrent.Future

object Webserver {
  def apply(): Unit = {
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
}
