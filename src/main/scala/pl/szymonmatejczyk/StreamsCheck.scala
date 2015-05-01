package pl.szymonmatejczyk

import akka.actor.{Cancellable, ActorSystem}
import akka.stream.scaladsl.OperationAttributes.InputBuffer
import akka.stream._
import akka.stream.scaladsl.FlowGraph.Implicits
import akka.stream.scaladsl._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by szymonmatejczyk on 22.04.15.
 */
object StreamsCheck {
  def time(): Long = {
    System.currentTimeMillis()
  }

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("sys")
    implicit val materializer = ActorFlowMaterializer()

    eachSecond(Iterator.from(1), 1.second).runWith(Sink.foreach(println)).onComplete(_ => system.shutdown())
  }

  def eachSecond[T](it: Iterator[T], interval: FiniteDuration): Source[(T, Long), Unit] = {
    val startT = time()

    val d = FlowGraph.partial() { implicit b: FlowGraph.Builder =>
      import Implicits._

      val tickS = b.add(Source.apply(1.second, 1.second, ()).map(_ => time() - startT))

      val conflate = b.add(Flow[T].conflate(seed = identity)((lastMessage, newMessage) => newMessage))

      val z =Zip[T, Long](OperationAttributes.inputBuffer(1, 1))
      val zip = b.add(z)

      conflate ~> zip.in0

      tickS ~> zip.in1

      UniformFanInShape(zip.out, conflate.inlet)
    }

    val source = Source.apply(() => it)

    Source() { implicit b: FlowGraph.Builder =>
      import Implicits._
      val f = b.add(d)
      source ~> f
      f.out
    }
  }
}
