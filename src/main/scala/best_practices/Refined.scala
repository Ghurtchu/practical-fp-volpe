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


    import eu.timepit.refined.api.RefinedTypeOps
    import eu.timepit.refined.numeric.Greater

    type GTFive = Int Refined Greater[5]
    object GTFive extends RefinedTypeOps[GTFive, Int]

    val number: Int = 33

    val intRes: Either[String, GTFive] = GTFive.from(number)

    // Summarizing, Refined lets us perform runtime validation via Either, which forms a Monad
    // this means := validation is done sequentially. it would fail on the first error encountered during
    // multiple value validation. In such cases it is usually a better choice to go for
    // cats.data.Validated, which is similar to Either, except it only forms an Applicative

    /**In practical terms, this means it can validate data simultaneously and accumulate errors
     * instead of validating data sequentially and failing fast on the first encountered error.
     * A common type for such purpose is ValidatedNel[E, A],
     * which is an alias for Vali- dated[NonEmptyList[E], A].
     * We can convert those refinement results to this type via the toValidatedNel extension method.
     */
  }
}
