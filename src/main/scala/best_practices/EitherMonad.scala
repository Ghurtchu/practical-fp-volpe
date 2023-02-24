package best_practices

import cats.{ApplicativeThrow, Functor}
import cats.syntax.functor._
import cats.syntax.applicativeError._

object EitherMonad {

  trait Item

  trait Items[F[_]] {
    def maybeFindAll: F[Either[Items.Search.Error, List[Item]]]
  }

  object Items {
    object Search {
      sealed trait Error

      object Error {
        case object RandomError extends Error
      }
    }
  }

  class Program[F[_]: Functor](items: Items[F]) {
    def findAll: F[List[Item]] =
      items.maybeFindAll.map {
        case Right(items) => items
        case _ => List.empty[Item]
      }
  }

}
