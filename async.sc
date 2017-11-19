import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

//future encodes latency in program
def fetch(url: String):Future[String] = {
  Thread.sleep(100)
  Future { "Future Result" }
}
//future value is extracted using polling
val futureResult = fetch("url")
futureResult.onComplete {
  case Success(s) => println(s)
  case Failure(f) => println(f)
}
Await.result(futureResult, 500.millis)


val p = Promise[String]
Future {
  Thread.sleep(100)
  p.success("Promise Result")
}
val promiseResult = p.future
promiseResult.onComplete {
  case Success(s) => println(s)
  case Failure(f) => println(f)
}
Await.result(promiseResult, 500.millis)


def future[T](block: => T): Future[T] = {
  val promise = Promise[T]

  global.execute(new Runnable {
    override def run(): Unit = try {
      promise.success(block)
    } catch {
      case NonFatal(e) => promise.failure(e)
    }
  })

  promise.future
}


