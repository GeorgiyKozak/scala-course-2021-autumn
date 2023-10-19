package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import Homework._

object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.{given Arbitrary[Zero],
                      given Arbitrary[Nat]}

  property("сheck isZero") = forAll { (zero: Zero) ⇒ zero.isZero }

  property("throw exception due to Zero doesn't have a predecessor") = forAll { (zero: Zero) ⇒
    try { zero.predecessor; false }
    catch { case e if classOf[Exception].isInstance(e) => e.getMessage == "0 doesn't have a predecessor" }
  }

  property("check addition to Zero") = forAll { (zero: Zero, nat: Nat) ⇒ zero + nat == nat }

  property("check subtraction from Zero") = forAll { (zero: Zero, nat: Nat) ⇒
    if nat.isZero then zero - nat == zero
    else
      try { zero - nat; false }
      catch {
        case e if classOf[Exception].isInstance(e) =>
          e.getMessage == "0 doesn't follow any natural number"
      }
  }

  property("check Zero to int conversion") = forAll { (zero: Zero) ⇒ zero.toInt == 0 }

  property("check equality to Zero") = forAll { (zero: Zero, nat: Nat) ⇒
    if nat.isZero then zero == nat
    else !(zero == nat)
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):

  ???

end SuccSpecification

object NatSpecification extends Properties("Nat"):
 
  ???

end NatSpecification
  