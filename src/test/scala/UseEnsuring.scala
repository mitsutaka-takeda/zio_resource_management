import org.scalatest.WordSpecLike
import scalaz.zio._

class UseEnsuring extends WordSpecLike with DefaultRuntime {
  "ensuring" should {
    "release" in {
      unsafeRun(
        ZIO.effectTotal(println("acquire resource"))
          .ensuring(UIO.effectTotal(println("release resource")))
      )
    }
  }

  "ensuring" when {
    "failure happened" should {
      "release resource" in {
        assertThrows[FiberFailure](
          unsafeRun(
            (for {
              _ <- UIO.effectTotal(println("acquire resource"))
              _ <- IO.fail("Error!")
              _ <- UIO.effectTotal(println("Does not reach this line due to failure"))
            } yield ()).ensuring(
              UIO.effectTotal(println("release resource even after failure"))
            )))
      }
    }
  }
}