package scala.meta.metals

import io.circe.Json
import io.circe.generic.JsonCodec
import org.langmeta.io.AbsolutePath
import java.net.URI
import scala.util.Try

@JsonCodec case class ActiveJson(uri: String, tokenfilePath: Option[String]) {
   def toURI: Either[Throwable, URI] = Try(URI.create(uri)).toEither
}

@JsonCodec case class SettingParams(setting: String)
@JsonCodec case class SettingResult(value: Json, contentType: Json)

@JsonCodec case class SbtInitializeParams(
    initializationOptions: Json = Json.obj()
)
@JsonCodec case class SbtInitializeResult(json: Json)

@JsonCodec case class SbtExecParams(commandLine: String)

case class MissingActiveJson(path: AbsolutePath)
    extends Exception(s"sbt-server 1.1.0 is not running, $path does not exist")
case class SbtServerConnectionError(msg: String) extends Exception(msg)
