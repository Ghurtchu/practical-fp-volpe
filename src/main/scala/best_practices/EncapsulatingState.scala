package best_practices

import cats.{Monad, catsInstancesForId}
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.implicits.catsSyntaxApplicative

object EncapsulatingState {

  // Tip: Out interface should know nothing about state

  object in_memory_counter {

    trait Counter[F[_]] {
      def incr: F[Unit]
      def get: F[Int]
    }

    import cats.Functor
    import cats.effect.kernel.Ref
    import cats.syntax.functor._
    import cats.syntax.flatMap._

    object Counter {
      // by exposing a smart constructor, we make it impossible for the Ref to be
      // accessed outside of it. This has a fundamental reason: state shall not leak.
      def make[F[_]: Functor: Ref.Make]: F[Counter[F]] =
        Ref.of[F, Int](0).map { ref =>
          new Counter[F] {
            override def incr: F[Unit] = ref.update(_ + 1)
            override def get: F[Int] = ref.get
          }
        }

      // a new counter will be created on every flatMap call, because
      // it will be IO[Counter[IO]].flatMap { counter => ??? }
    }

    trait Console[F[_]] {
      def print(input: String): F[Unit]
    }

    object Console {
      def apply[F[_]](implicit console: Console[F]): Console[F] = console
    }

    implicit class ConsoleSyntax[A](self: A) {
      def print[F[_]](implicit console: Console[F]): F[Unit] = console.print(self.toString)
    }

    implicit val consoleInt: Console[IO] = IO.println(_)

    def genericProgram[F[_] : Monad : Console](counter: Counter[F]): F[Unit] = {
      for {
        _ <- counter.get.flatMap(i => Console[F].print(i.toString))
        _ <- counter.incr
        _ <- counter.get.flatMap(i => Console[F].print(i.toString))
      } yield ()
    }


    // if you don't like anonymous instances, you can create named class

    class LiveCounter[F[_]] private (ref: Ref[F, Int]) extends Counter[F] {
      override def incr: F[Unit] = ref.update(_ + 1)
      override def get: F[Int] = ref.get
    }


    object LiveCounter {
      def make[F[_]: Sync]: F[Counter[F]] =
        Ref.of[F, Int](0).map(new LiveCounter[F](_))
    }

    def program(counter: Counter[IO]): IO[Unit] = {
      for {
        _ <- counter.get.flatMap(IO.println)
        _ <- counter.incr
        _ <- counter.get.flatMap(IO.println)
        _ <- counter.incr.replicateA(5).void
        _ <- counter.get.flatMap(IO.println)
      } yield ()
    }

  }

}
