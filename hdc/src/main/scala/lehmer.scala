package io.f1r3fly.rhohdc.tinyrho

import hv.*
import be.adamv.macroloop.collection.SizedVector

import scala.collection.mutable.ArrayBuffer
import CompilerWorldState._

// Example usage
// val permutation = Array(3, 1, 4, 2)
// val code = lehmerCode(permutation)
// println(s"Lehmer code of $permutation is: $code")

// This implementation defines a factorial function to compute the
// factorial of a given number. The lehmerCode function takes an array
// representing a permutation and returns its Lehmer code as an
// integer. It iterates over the elements of the permutation and
// counts the number of elements that are smaller than the current
// element but appear later in the array. It multiplies the count by
// the factorial of the remaining elements and adds it to the
// code. Finally, it returns the Lehmer code.

// In the Lehmer code for permutations we could replace the factorial
// calculation with a symbol, a variable that represents the factorial
// calculation, then the code changes from an integer to a
// polynomial. Using this insight, you rewrite the encode and
// decode for this new Lehmer polynomial code.

// Example usage
// val permutation = Array(3, 1, 4, 2)
// val code = encodeLehmerCode(permutation)
// println(s"Lehmer polynomial code for $permutation is: $code")

// val decodedPermutation = decodeLehmerCode(code)
// println(s"Decoded permutation for code $code is: ${decodedPermutation.mkString(", ")}")

// Example usage
// val coefficients = Array(2, 0, 3, 1, 0)
// val bitVector = encodeLehmerCodeToBitVector(coefficients)
// println(s"Bit vector representation of Lehmer polynomial $coefficients is: $bitVector")

// val decodedCoefficients = decodeLehmerCodeFromBitVector(bitVector)
// println(s"Decoded Lehmer polynomial coefficients from bit vector $bitVector: ${decodedCoefficients.mkString(", ")}")

object Lehmer {
  def factorial(n: Int): Int = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  def encode(permutation: Array[Int]): Int = {
    val n = permutation.length
    var lc = 0
    for (i <- 0 until n) {
      var count = 0
      for (j <- i + 1 until n) {
        if (permutation(j) < permutation(i)) {
          count += 1
        }
      }
      lc += count * factorial(n - i - 1)
    }
    lc
  }

  def decode(code: Int): Array[Int] = {
    val n = code.toString.length
    val availableNumbers = Range(0, n+1).toBuffer
    val permutation = Array.fill(n)(0)

    var cv = code

    for (i <- 0 until n) {
      val index = cv / factorial(n - i - 1)
      permutation(i) = availableNumbers(index)
      availableNumbers.remove(index)
      cv = (cv % factorial(n - i - 1))
    }

    permutation
  }

  // i suspect there is a bug in this implementation. i suspect that
  // we get multiple permutations with the same polynomial unless we
  // note specifically which factorial factor the variable represents.
  def encode2Polynomial(permutation: Array[Int]): String = {
    val n = permutation.length
    val coefficients = Array.fill(n)(0)

    for (i <- 0 until n) {
      var count = 0
      for (j <- i+1 until n) {
        if (permutation(j) < permutation(i)) {
          count += 1
        }
      }
      coefficients(i) = count
    }
  
    coefficients.mkString(" ")
  }

  def decode2Polynomial(code: String): Array[Int] = {
    val coefficients = code.split(" ").map(_.toInt)
    val n = coefficients.length
    val availableNumbers = Range(0, n+1).toBuffer
    val permutation = Array.fill(n)(0)

    for (i <- 0 until n) {
      val index = coefficients(i)
      permutation(i) = availableNumbers(index)
      availableNumbers.remove(index)
    }

    permutation
  }

  def encodeLehmerCodeToBitVector(coefficients: Array[Int]): String = {
    val bitVector = new ArrayBuffer[Boolean]()
  
    for (i <- 0 until coefficients.length) {
      for (j <- 0 until coefficients(i)) {
        bitVector += false
      }
      bitVector += true
    }
  
    bitVector.map(if (_) '1' else '0').mkString("")
  }

  def decodeLehmerCodeFromBitVector(bitVector: String): Array[Int] = {
    val coefficients = new ArrayBuffer[Int]()
    var count = 0

    bitVector.foreach { bit =>
      if (bit == '0') {
        count += 1
      } else {
        coefficients += count
        count = 0
      }
    }

    coefficients.toArray
  }

}

// In this implementation, the permutationToLehmerCode function takes
// an array representing a permutation and generates the Lehmer code
// for that permutation. It iterates through the elements of the
// permutation and calculates the inversion count for each element,
// storing it in the lehmerCode array. The Lehmer code is then
// converted to a string using the mkString method.

// The lehmerCodeToPermutation function takes a string representation
// of the Lehmer code and generates the corresponding permutation. It
// parses the code, then iterates through the elements to reconstruct
// the permutation.

// In the main method, an example permutation is used to demonstrate
// the functionality. The permutation [2, 4, 1, 5, 3] is converted to
// its Lehmer code representation and then decoded back to the
// original permutation.


object LehmerCodeVariant {  
  //def permutationToLehmerCode(permutation: Array[Int]): String = {
  def permutationToLehmerCode(perm: Permutation): String = {
    val permutation = perm.raw
    val n = permutation.length
    val lehmerCode = Array.ofDim[Int](n)
    
    for (i <- 0 until n) {
      var rank = permutation(i) - 1
      
      for (j <- 0 until i) {
        if (permutation(j) < permutation(i))
          rank -= 1
      }
      
      lehmerCode(i) = rank
    }
    
    lehmerCode.mkString(" ")
  }
  
  //def lehmerCodeToPermutation(code: String): Array[Int] = {
  def lehmerCodeToPermutation(code: String): Permutation = {
    val lehmerCode = code.split(" ").map(_.toInt)
    val n = lehmerCode.length
    val permutation = Array.ofDim[Int](n)
    val used = Array.ofDim[Boolean](n + 1)
    
    for (i <- 0 until n) {
      var count = lehmerCode(i) + 1
      var j = 1
      
      while (count > 0) {
        if (!used(j))
          count -= 1
        j += 1
      }
      
      while (used(j))
        j += 1
      
      permutation(i) = j
      used(j) = true
    }

    //printStdOut( s"""permutation: ${permutation}""" )
    
    try {
      Permutation.fromRaw( SizedVector.wrap( permutation ) )
    } catch {
      case _ : Throwable => {
        Permutation.random // BUGBUG -- just to get an end-to-end compilation
      }
    }
  }

  //def lehmerCodeToBitVector(lehmerCode: String): Array[Boolean] = {
  def lehmerCodeToBitVector(lehmerCode: String): HyperVector = {
    HyperVector.fromRaw(
      SizedVector.wrap(
        lehmerCode.split(" ").flatMap { digit =>
          val binaryString = (digit.toInt + 1).toBinaryString.reverse
          binaryString.tail.map(_ == '1') :+ (binaryString.head != '0')
        }.map(bool => if (bool) 1 else 0).toArray
      )
    )
  }

  //def bitVectorToLehmerCode(bitVector: Array[Boolean]): String = {
  def bitVectorToLehmerCode(bitVector: HyperVector): String = {
    val bits = bitVector.raw.iterator
    val lehmerCode = new StringBuilder

    while (bits.hasNext) {
      var digit = 0
      var power = 1
      var bit = bits.next()

      while ((bit != 0) && bits.hasNext) {
        digit += power
        power *= 2
        bit = bits.next()
      }

      lehmerCode.append(digit - 1).append(' ')
    }

    lehmerCode.toString().trim
  }
  
  // def main(args: Array[String]): Unit = {
  //   val permutation = Array(2, 4, 1, 5, 3)
  //   val lehmerCode = permutationToLehmerCode(permutation)
  //   println("Lehmer code: " + lehmerCode)
    
  //   val decodedPermutation = lehmerCodeToPermutation(lehmerCode)
  //   println("Decoded permutation: " + decodedPermutation.mkString(", "))
  // }
}
