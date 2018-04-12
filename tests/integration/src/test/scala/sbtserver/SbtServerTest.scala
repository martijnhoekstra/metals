package tests.sbtserver

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.meta.metals.sbtserver.Sbt
import scala.meta.metals.sbtserver.SbtServer
import scala.util.Failure
import scala.util.Success
import monix.execution.schedulers.TestScheduler
import org.langmeta.internal.io.PathIO
import org.langmeta.jsonrpc.Services
import org.langmeta.lsp.TextDocument
import org.langmeta.lsp.Window
import tests.MegaSuite

case class SbtServerConnectionError(msg: String) extends Exception(msg)

object SbtServerTest extends MegaSuite {
  implicit val s = TestScheduler()
  test("correct sbt 1.1 project establishes successful connection") {
    val services = Services.empty
      .notification(Window.logMessage)(msg => ())
      .notification(TextDocument.publishDiagnostics)(msg => ())
    val program = for {
      sbt <- SbtServer.connect(PathIO.workingDirectory, services).map {
        case Left(err) => throw SbtServerConnectionError(err)
        case Right(ok) =>
          println("Established connection to sbt server.")
          ok
      }
      response <- Sbt.setting.query("metals/crossScalaVersions")(sbt.client)
    } yield {
      val Right(json) = response
      val Right(crossScalaVersions) = json.value.as[List[String]]
      sbt.runningServer.cancel()
      assertEquals(crossScalaVersions, List("2.12.4"))
      crossScalaVersions
    }
    val future = program.materialize.runAsync
    s.tick(5 seconds)
    val result = Await.result(future, Duration(5, "s"))
    result match {
      case Success(_) => // hurrah :clap:
      case Failure(err) => throw err
    }
  }

}
