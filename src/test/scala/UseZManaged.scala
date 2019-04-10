import org.scalatest.{Matchers, WordSpecLike}
import scalaz.zio._

class MyResource2(var closed: Boolean = false) {
  def close(): Unit = {
    closed = true
  }

  def doSomething: Boolean = {
    require(!closed)
    true
  }
}

class MyResource3(var closed: Boolean = false) {
  def close(): Unit = {
    closed = true
  }

  def doSomething: Boolean = {
    require(!closed)
    true
  }
}

class UseZManaged extends WordSpecLike with Matchers with DefaultRuntime {

  val myResource2IO: Task[ZManaged[Any, Nothing, MyResource2]] =
    ZIO.effect(
      ZManaged.make(ZIO.succeed(new MyResource2)) { m => UIO.effectTotal(m.close()) }
    )

  "Resource" should {
    "not outlive its scope" in {
      unsafeRun(
        for {
          myResource <- myResource2IO
          r <- myResource.use(
            r =>
              for {
                b <- ZIO.succeed(r.doSomething)
              } yield b
          )
        } yield r
      ) shouldBe true
    }
  }

  val multipleResourcesIO: Task[ZManaged[Any, Nothing, (MyResource3, MyResource2)]] = ZIO.effect(
    ZManaged.make(ZIO.succeed(new MyResource3)) { m => UIO.effectTotal(m.close())}.zipPar{
      ZManaged.make(ZIO.succeed(new MyResource2)) { m => UIO.effectTotal(m.close()) }
    }
  )

  "ZManaged" should {
    "combine multiple resources" in {
      unsafeRun(for {
        multipleResources <- multipleResourcesIO
        result <- multipleResources.use{
          case (r2, r3) =>
            UIO.succeed(r2.doSomething && r3.doSomething)
        }
      } yield result) shouldBe true
    }
  }
}