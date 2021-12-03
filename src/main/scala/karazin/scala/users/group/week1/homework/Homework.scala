package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec

/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean = if b then false else true

    def and(left: Boolean, right: Boolean): Boolean = if left then right else false

    def or(left: Boolean, right: Boolean): Boolean = if left then true else right

  end `Boolean Operators`

  object `Fermat Numbers`:

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) => {
      @tailrec
      def multImpl(a: BigInt, b: BigInt, acc: BigInt): BigInt = {
        if b == 0 then acc
        else multImpl(a, b - 1, acc + a)
      }

      multImpl(a, b, 0)
    }

    val power: (BigInt, BigInt) => BigInt = (a, n) => {
      @tailrec
      def powerImpl(a: BigInt, n: BigInt, acc: BigInt): BigInt = {
        if n == 0 then acc
        else powerImpl(a, n - 1, multiplication(acc, a))
      }

      powerImpl(a, n, 1)
    }

    val fermatNumber: Int => BigInt = n => power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence`:

    def toLookAndSay(curSequenceElement: BigInt): BigInt = {
      //splitting a number into its digits
      val list = curSequenceElement.toString().map(_.asDigit).toList

      @tailrec
      def implementation(digits: List[Int], curDigit: BigInt, count: BigInt, acc: BigInt): BigInt = {
        if digits == List() then (acc * 10 + count) * 10 + curDigit //viewed the number
        else {
          if digits.head == curDigit then implementation(digits.tail, curDigit, count + 1, acc)
          else implementation(digits.tail, digits.head, 1, (acc * 10 + count) * 10 + curDigit)
        }
      }

      implementation(list.tail, list.head, 1, 0)
    }

    val lookAndSaySequenceElement: Int => BigInt = n => {
      @tailrec
      def loop(cur: BigInt, left: BigInt): BigInt = {
        if left == 0 then cur
        else loop(toLookAndSay(cur), left - 1)
      }

      loop(1, n)
    }

  end `Look-and-say Sequence`

end Homework