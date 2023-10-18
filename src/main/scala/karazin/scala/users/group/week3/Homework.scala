package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = Succ(this)
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int) =
      require (int >= 0, "Negative number cannot be converted to Nat")
      @tailrec
      def fromIntRec(acc: Nat, int: Int): Nat =
        if int == 0 then acc
        else fromIntRec(acc.successor, int - 1)

      fromIntRec(Zero, int)

    override def toString: String = s"Nat($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat =
      if that.isZero then this
      else throw new Exception("0 doesn't follow any natural number")
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match
      case zero: Zero ⇒ true
      case _          ⇒ false

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat = n + that.successor
    
    infix def -(that: Nat): Nat =
      if that.isZero then this
      else n - that.predecessor
    
    // Optional task
    def toInt: Int =
      @tailrec
      def toIntRec(nat: Nat, acc: Int): Int =
        if nat.isZero then acc
        else toIntRec(nat.predecessor, acc + 1)

      toIntRec(n, acc = 1)

    override def equals(obj: Any): Boolean = obj match
      case zero: Zero ⇒ false
      case nat: Nat   ⇒ this.successor == nat.successor
      case _          ⇒ false


