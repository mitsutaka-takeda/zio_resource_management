import org.scalatest.WordSpecLike
import scalaz.zio.{DefaultRuntime, FiberFailure, UIO, ZIO}
import java.lang._

class MyResource1(var closed: Boolean = false) {
  def close(): Unit = {
    closed = true
  }

  def doSomething: Boolean = {
    require(!closed)
    true
  }
}

class ResourceFailedToInitialize() {
  throw new Exception("Initialization failure")

  def close(): Unit = {
    println("closed should not be called")
  }

  def doSomething(): Unit = ()
}

class UseBracket extends WordSpecLike with DefaultRuntime {

  "bracket" should {
    "release" in {
      unsafeRun(
        ZIO.effect(new MyResource1).bracket(r => UIO.effectTotal(r.close())) {
          resource =>
            UIO.effectTotal(resource.doSomething)
        }
      )
    }
  }

  "bracket" when {
    "initialization failed" should {
      "not call release" in {
        assertThrows[FiberFailure] {
          unsafeRun(
            ZIO.effect(new ResourceFailedToInitialize)
              .bracket(r => UIO.effectTotal(r.close())) {
                resource =>
                  UIO.effectTotal(resource.doSomething())
              }
          )
        }
      }
    }
  }
}
