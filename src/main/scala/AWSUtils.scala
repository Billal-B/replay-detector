import java.io.File

import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.amazonaws.services.s3.model.{ObjectMetadata, PutObjectRequest}

object AWSUtils {

  def s3client() = {
    AmazonS3ClientBuilder
      .standard()
      .withRegion("eu-west-1")
      .withCredentials(new ProfileCredentialsProvider())
      .build()
  }

  def uploadToS3(filenames: Seq[String], bucketName: String, folder: String): Unit = {
    val client = s3client()
    filenames foreach {filename =>
      val file = new File(filename)
      val fullFolder = if (folder endsWith("/")) folder else folder + "/"
      val request = new PutObjectRequest(bucketName, fullFolder + file.getName, file)
      val metadata = new ObjectMetadata()
      metadata.setContentType("plain/text")
      request.setMetadata(metadata)
      s3client().putObject(request)
    }
  }
}