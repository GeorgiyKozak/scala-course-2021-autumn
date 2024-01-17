package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import Homework._

object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)
  include(NatSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.{given Arbitrary[Zero],
                      given Arbitrary[Nat]}

  property("check isZero on Zero") = forAll { (zero: Zero) ⇒ zero.isZero }

  property("throw exception due to Zero doesn't have a predecessor") = forAll { (zero: Zero) ⇒
    try { zero.predecessor; false }
    catch {
      case e if classOf[Exception].isInstance(e) =>
        e.getMessage == "0 doesn't have a predecessor"
    }
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
  import arbitraries.{given Arbitrary[Succ],
                      given Arbitrary[Nat],
                      given Arbitrary[Int]}

  property("check isZero on Succ") = forAll { (succ: Succ) ⇒ !succ.isZero }

  property("check Succ to Int conversion") = forAll { (n: Int) ⇒
    Nat.fromInt(n).toInt == n
  }

  property("check predecessor on Succ") = forAll { (succ: Succ, nat: Nat) ⇒
    succ.predecessor.toInt == succ.toInt - 1 &&
      succ.predecessor == succ - Succ(Zero)
  }

  property("check addition to Succ") = forAll { (succ: Succ, nat: Nat) ⇒
    (succ + nat).toInt == succ.toInt + nat.toInt
  }

  property("check subtraction from Succ") = forAll { (succ: Succ, nat: Nat) ⇒
    if nat.toInt <= succ.toInt then (succ - nat).toInt == succ.toInt - nat.toInt
    else
      try { (succ - nat).toInt == succ.toInt - nat.toInt; false }
      catch {
        case e if classOf[Exception].isInstance(e) ⇒
          e.getMessage == "0 doesn't follow any natural number"
      }
  }

  property("check equality to Succ") = forAll { (succ: Succ, nat: Nat) ⇒
    (succ == nat) == (succ.toInt == nat.toInt)
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  import arbitraries.{given Arbitrary[Nat]}

  property("check isZero on Nat") = forAll { (nat: Nat) ⇒
    nat.isZero == (nat match
      case _: Zero ⇒ true
      case _       ⇒ false)
  }

  property("check fromInt and toInt conversions") = forAll { (nat: Nat) ⇒
    Nat.fromInt(nat.toInt) == nat
  }

  property("check predecessor of Nat") = forAll { (nat: Nat) ⇒
    if !nat.isZero then nat.predecessor == nat - Succ(Zero) && nat.predecessor.toInt == nat.toInt - 1
    else
      try { nat.predecessor; false }
      catch {
        case e if classOf[Exception].isInstance(e) ⇒
          e.getMessage == "0 doesn't have a predecessor"
      }
  }

  property("check successor of Nat") = forAll { (nat: Nat) ⇒
    nat.successor == Succ(nat) &&
      nat.successor.toInt == nat.toInt + 1
  }

  property("check addition to Nat") = forAll { (left: Nat, right: Nat) ⇒
    (left + right).toInt == left.toInt + right.toInt
  }

  property("check subtraction from Nat") = forAll { (left: Nat, right: Nat) ⇒
    if right.toInt <= left.toInt then (left - right).toInt == left.toInt - right.toInt
    else
      try { (left - right).toInt == left.toInt - right.toInt; false }
      catch {
        case e if classOf[Exception].isInstance(e) ⇒
          e.getMessage == "0 doesn't follow any natural number"
      }
  }


end NatSpecification