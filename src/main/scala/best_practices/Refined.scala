package best_practices

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.collection.{Contains, NonEmpty}
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros.newtype

object Refined extends App {

  trait User
  type Username = String Refined Contains['a']
  def lookup(username: Username): Option[User] =
    Some(new User {})

  lookup("aa") // compiles
  // lookup("b") // compile time error

  type Input = NonEmptyString
  def processInput(input: Input): Unit = ()

  processInput("asbc") // compiles
  // processInput("") // does not compile

  // newtype and refined together
  // zero cost wrapper + each has its own type + compile time validation
  @newtype case class Brand(value: NonEmptyString)
  @newtype case class Category(value: NonEmptyString)

  Brand("Boom") // compiles
  // Brand("") // does not compile

  object runtime_validation {

    val str: String = "some runtime value"

    val res: Either[String, NonEmptyString] =
      refineV[NonEmpty](str)

    val resAlternative: Either[String, NonEmptyString] =
      NonEmptyString.from(str)

  }
}
