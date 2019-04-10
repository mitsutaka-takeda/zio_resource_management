import org.scalatest.{Matchers, WordSpecLike}
import scalaz.zio._

class MyResource(var closed: Boolean) {
  def close(): Unit = {
    closed = true
  }

  def doSomething: Boolean = {
    require(!closed)
    true
  }
}

class GotchaWithResourceManagement extends WordSpecLike with Matchers with DefaultRuntime {

  val myResource: ZManaged[Any, Nothing, MyResource] = ZManaged.make(ZIO.succeed({
    new MyResource(false)
  })) { m => UIO.effectTotal(m.close()) }

  "ZManaged" should {
    "double free" in {
      assertThrows[FiberFailure](
        unsafeRun(
          for {
            result1 <- myResource.use(m => UIO.effectTotal(m.doSomething))
            result2 <- myResource.use(m => UIO.effectTotal(m.doSomething))
          } yield result1 && result2
        ))
    }
  }

  val myResourceIO: UIO[ZManaged[Any, Nothing, MyResource]] = ZIO.effectTotal(ZManaged.make(ZIO.succeed({
    new MyResource(false)
  })) { m => UIO.effectTotal(m.close()) })

  "ZManaged" should {
    "not double free" in {
      unsafeRun(
        for {
          resource1 <- myResourceIO
          resource2 <- myResourceIO
          result1 <- resource1.use(m => UIO.effectTotal(m.doSomething))
          result2 <- resource2.use(m => UIO.effectTotal(m.doSomething))
        } yield result1 && result2
      ) shouldBe true
    }
  }
}
