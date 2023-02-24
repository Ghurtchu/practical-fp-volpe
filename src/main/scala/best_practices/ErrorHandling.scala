package best_practices

import cats.MonadThrow
import cats.effect.std.Random
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import scala.util.control.NoStackTrace

object ErrorHandling {

  trait Category

  trait Categories[F[_]] {
    def findAll: F[List[Category]]
  }

  object Categories {
    def make[F[_]: MonadThrow : Random]: Categories[F] = new Categories[F] {
      override def findAll: F[List[Category]] =
        Random[F].nextInt.flatMap {
          case n if n > 100 => List.empty[Category].pure[F]
          case _ => BusinessError.RandomError.raiseError[F, List[Category]]
        }
    }
  }

  sealed trait BusinessError extends NoStackTrace

  object BusinessError {
    type RandomError = RandomError.type
    case object RandomError extends BusinessError
  }

}
