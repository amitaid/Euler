import scala.collection.immutable.HashSet
import scala.io.Source
import scala.math.BigInt

object Euler extends App {

  // Euler 1
  lazy val e1 = (1 until 1000) filter (x => x % 3 == 0 || x % 5 == 0) sum

  // Euler 2
  lazy val fibs: Stream[BigInt] = 0 #:: 1 #:: (fibs zip fibs.tail).map { t => t._1 + t._2 }
  lazy val e2 = fibs filter (_ % 2 == 0) takeWhile (_ < 4000000) sum

  // Euler 3
  lazy val num = 600851475143L
  // Constructing the stream from the first odd number below the num's sqrt
  // Descending in leaps of 2
  // Can be improved to check for (6*i +- 1)
  lazy val desc: Stream[Long] = (math.sqrt(num).floor.toLong - 1) #:: desc.map(_ - 2)
  lazy val e3 = desc.find(x => num % x == 0 && isPrime(x))

  // Euler 4
  // Can be improved greatly
  def palindrome(x: Int): Boolean = x.toString == x.toString.reverse

  lazy val e4 = (for (i <- 999 to 100 by -1;
                      j <- 999 to 100 by -1
                      if palindrome(i * j)) yield i * j).max

  // Euler 5
  def primeFactors(n: Int) = {
    def primeHelper(num: Int, x: Int, factors: List[Int]): List[Int] =
      if (num < 2) factors.reverse
      else if (num % x == 0) primeHelper(num / x, x, x :: factors)
      else primeHelper(num, x + 1, factors)

    primeHelper(n, 2, List()).groupBy(x => x).mapValues(_.size)
  }

  lazy val e5 = (2 to 20).flatMap(x => primeFactors(x))
    .foldLeft(Map[Int, Int]())((m, fac) => m.updated(fac._1, math.max(fac._2, m.getOrElse(fac._1, 0))))
    .map { case (k, v) => math.pow(k, v).toInt }.product

  // Euler 6
  lazy val e6 = (math.pow((1 to 100).sum, 2) - (1 to 100).map(x => x * x).sum).toLong

  // Euler 7
  lazy val primes: Stream[Long] = 2 #:: sieve(3)
  def sieve(x: Long): Stream[Long] = {
    if (primes.takeWhile(p => p * p <= x).exists(x % _ == 0)) sieve(x + 2)
    else x #:: sieve(x + 2)
  }
  def isPrime(n: Long) = primes.dropWhile(_ < n).head == n

  lazy val e7 = primes(10000)

  // Euler 8
  lazy val number = Source.fromURL(getClass.getResource("euler8.txt")).getLines().mkString("")
  lazy val e8 = (0 to (number.length - 13)).map(i => number.substring(i, i + 13).map(_.asDigit.toLong).product).max

  // Euler 9
  lazy val e9 = (for {x <- 1 to 499
                      y <- x + 1 to 500
                      z = 1000 - x - y
                      if x * x + y * y == z * z} yield x * y * z).head

  // Euler 10
  lazy val e10 = primes.takeWhile(_ < 2000000).sum

  // Euler 11
  lazy val matrix = Source.fromURL(getClass.getResource("euler11.txt")).getLines().map(_.split(" ").map(_.toLong)).toArray

  def maxProductAt(y: Int, x: Int, m: Array[Array[Long]]): Long = {
    val vertical: Long = if (y < m.length - 4 && x < m(y).length) m(y)(x) * m(y + 1)(x) * m(y + 2)(x) * m(y + 3)(x) else 0
    val horizontal: Long = if (y < m.length && x < m(y).length - 4) m(y)(x) * m(y)(x + 1) * m(y)(x + 2) * m(y)(x + 3) else 0
    val diag1: Long = if (y < m.length - 4 && x < m(y).length - 4)
      m(y)(x) * m(y + 1)(x + 1) * m(y + 2)(x + 2) * m(y + 2)(x + 3)
    else 0
    val diag2: Long = if (y < m.length - 4 && x < m(y).length - 4)
      m(y + 3)(x) * m(y + 2)(x + 1) * m(y + 1)(x + 2) * m(y)(x + 3)
    else 0
    List(vertical, horizontal, diag1, diag2).max
  }

  lazy val e11 = (for {y <- matrix.indices
                       x <- matrix(y).indices} yield maxProductAt(y, x, matrix)).max

  // Euler 12
  lazy val triNum = Stream.from(2).find(n => primeFactors((1 to n).sum).map { case (k, v) => v + 1 }.product > 500)
  lazy val e12 = (1 to triNum.get).sum

  // Euler 13
  lazy val numbers = Source.fromURL(getClass.getResource("euler13.txt")).getLines().toArray
  lazy val e13 = numbers.map { s => BigInt(s.substring(0, 12)) }.sum.toString().substring(0, 10)

  // Euler 14
  def collatz(n: Long): Long = {
    def collatzRec(n: Long, res: Long): Long = {
      if (n <= 1) res + 1
      else {
        val next = if (n % 2 == 0) n / 2 else n * 3 + 1
        collatzRec(next, res + 1)
      }
    }
    collatzRec(n, 0)
  }

  lazy val e14 = (1 until 1000000).maxBy(n => collatz(n))

  // Euler 15
  def subFact(from: Int, to: Int): BigInt = {
    def factRec(f: Int, t: Int, res: BigInt): BigInt = {
      if (t < f) res
      else factRec(f, t - 1, res * t)
    }
    factRec(from, to, 1)
  }
  def fact(n: Int) = subFact(1, n)

  lazy val e15 = subFact(21, 40) / fact(20)  // 137846528820

  // Euler 16
  lazy val e16 = List.fill(1000)(BigInt(2)).product.toString().map(_.asDigit).sum

  // Euler 17
  def numToLength(n: Int) = n match {
    case 0 => 4 // Zero
    case 1 => 3 // One
    case 2 => 3 // Two
    case 3 => 5 // Three
    case 4 => 4 // Four
    case 5 => 4 // Five
    case 6 => 3 // Six
    case 7 => 5 // Seven
    case 8 => 5 // Eight
    case 9 => 4 // Nine
    case _ => throw new IllegalArgumentException
  }
  def tensToLengthOne(n: Int) = n match {
    case 0 => 3 // Ten
    case 1 => 6 // Eleven
    case 2 => 6 // Twelve
    case 3 => 8 // Thirteen
    case 4 => 8 // Fourteen
    case 5 => 7 // Fifteen
    case 6 => 7 // Sixteen
    case 7 => 9 // Seventeen
    case 8 => 8 // Eighteen
    case 9 => 8 // Nineteen
  }
  def tensToLength(n: Int) = n match {
    case 2 => 6 // Twenty
    case 3 => 6 // Thirty
    case 4 => 5 // Forty
    case 5 => 5 // Fifty
    case 6 => 5 // Sixty
    case 7 => 7 // Seventy
    case 8 => 6 // Eighty
    case 9 => 6 // Ninety
  }

  def numToLetters(n: Int): Int = {
    val digits = (n.toString.reverse + "000").map(_.asDigit)
    if (digits.forall(_ == 0)) numToLength(n)
    else {
      var sum = 0
      if (digits(3) != 0) sum += numToLength(digits(3)) + "Thousand".length
      if (digits(2) != 0) sum += numToLength(digits(2)) + "Hundred".length
      if (digits(1) == 1) sum += tensToLengthOne(digits.head)
      else {
        if (digits(1) != 0) sum += tensToLength(digits(1))
        if (digits.head != 0) sum += numToLength(digits.head)
      }
      if ((digits(3) != 0 || digits(2) != 0) && (digits(1) != 0 || digits.head != 0)) sum += 3
      sum
    }
  }
  lazy val e17 = (1 to 1000).map(n => numToLetters(n)).sum

  // Euler 18
  lazy val triangle: Array[Array[Int]] =
    Source.fromURL(getClass.getResource("euler18.txt")).getLines().toArray.map(_.split(" ").map(_.toInt))
  var triangleMemo: Map[(Int, Int), Int] = Map()
  def maxPath(h: Int, x: Int, t: Array[Array[Int]], memo: Map[(Int, Int), Int]): Int = {
    if (!triangleMemo.contains((h, x)))
      triangleMemo += (h, x) ->
        (t(h)(x) + (if (h == t.length - 1) 0 else math.max(maxPath(h + 1, x, t, memo), maxPath(h + 1, x + 1, t, memo))))
    triangleMemo((h, x))
  }
  lazy val e18 = maxPath(0, 0, triangle, triangleMemo)

  // Euler 19
  def isLeap(year: Int) = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)

  def daysInMonth(month: Int, year: Int) = month match {
    case 1 => 31
    case 2 => if (isLeap(year)) 29 else 28
    case 3 => 31
    case 4 => 30
    case 5 => 31
    case 6 => 30
    case 7 => 31
    case 8 => 31
    case 9 => 30
    case 10 => 31
    case 11 => 30
    case 12 => 31
  }

  def daysInYear(year: Int) = 365 + (if (isLeap(year)) 1 else 0)

  // TODO

  // Euler 20
  lazy val e20 = fact(100).toString().toCharArray.map(_.asDigit).sum

  // Euler 21
  def d(n: Int) = (1 until n).filter(n % _ == 0).sum

  // Divisors
  lazy val e21 = (1 until 10000).filter(x => {
    val y = d(x); x != y && x == d(y)
  }).sum

  // Euler 22
  lazy val names = Source.fromURL(getClass.getResource("p022_names.txt")).mkString.replaceAll("\"", "").split(",")
  lazy val e22 = names.sorted.map(_.toCharArray.map(_ - 'A' + 1).sum).
    zip(Stream.from(1)).map { case (name, index) => name * index }.sum

  // Euler 23
  def isAbundant(n: Int) = d(n) > n

  lazy val abundant = (1 to 28123).filter(isAbundant).toSet
  lazy val e23 = (1 to 28123).filter(x => abundant.forall(a => !abundant.contains(x - a))).sum

  // Euler 24
  lazy val e24 = "0123456789".permutations.drop(999999).next()

  // Euler 25
  lazy val e25 = fibs.zip(Stream.from(0)).find(_._1.toString().length == 1000).get._2

  // Euler 26
  lazy val e26 = primes.takeWhile(_ < 1000).map(_.toInt).map(d => (1 to d).filter(n => math.pow(10, n) % d == 1))
  // TODO

  // Euler 27
  def quad(a: Int, b: Int, n: Int): Int = n * n + a * n + b

  lazy val ps = primes.takeWhile(_ < 1000).map(_.toInt)
  lazy val e27 = (for {b <- ps
                       a <- Range(-b, b, 2)
                       l = Stream.from(0).map(quad(a, b, _)).takeWhile(ps.contains(_)).length
  } yield (a * b, l, a, b)).maxBy(_._2)

  // Euler 28
  lazy val e28 = 1 + (1 to 500).map(n => 4 * (n * 2 + 1) * (n * 2 + 1) - 6 * 2 * n).sum

  // Euler 29
  lazy val limit = 100
  lazy val e29 = (for {a <- 2 to limit; b <- 2 to limit} yield math.pow(a, b)).distinct.size

  // Euler 30
  lazy val power = 5
  lazy val e30 = (10 to 1000000).filter(x => x.toString.toCharArray.map(c => math.pow(c.asDigit, power)).sum == x).sum

  // Euler 31
  lazy val coins = List(200, 100, 50, 20, 10, 5, 2, 1)

  def coinCombs(t: Int, cs: List[Int] = coins): Int = {
    if (t == 0) 1
    else if (t < 0 || cs.isEmpty) 0
    else coinCombs(t - cs.head, cs) + coinCombs(t, cs.tail)
  }

  lazy val e31 = coinCombs(200)

  // Euler 32
  lazy val e32 = (1 to 2).flatMap(n =>
    "123456789".permutations.
      filter(s => s.substring(0, n).toInt * s.substring(n, 5).toInt == s.substring(5).toInt).map(_.substring(5).toInt)
  ).distinct.sum

  // Euler 33
  lazy val e33 = (for {numer <- 10 to 99 map{_.toString}
                       denum <- numer.toInt to 99 map{_.toString}
                       d = numer.find(denum.contains(_))
                       if d.isDefined && numer.indexOf(d.get) != denum.indexOf(d.get)
                       numCut = numer.replaceFirst(d.get.toString, "").toInt
                       denumCut = denum.replaceFirst(d.get.toString, "").toInt
                       if d.get != '0' && numCut != 0 && denumCut != 0 &&
                         numer.toDouble / denum.toDouble == numCut.toDouble / denumCut
  } yield (numCut, denumCut)).foldLeft((1, 1))((res, cur) => (res._1 * cur._1, res._2 * cur._2))

  // Euler 34
  lazy val e34 = (3 to 1000000).filter(n => n.toString.toCharArray.map(c => fact(c.asDigit)).sum.toInt == n).sum

  // Euler 35
  lazy val primes35 = primes.takeWhile(_ < 1000000).toSet
  def circular(n: Int) = {
    val numStr = n.toString
    (0 until numStr.length).map(i => (numStr.substring(i) + numStr.substring(0, i)).toLong).toSet
  }
  lazy val e35 = (2 to 1000000).map(circular).count(_.subsetOf(primes35))

  // Euler 36
  def isPalindrome(s: String): Boolean = s == s.reverse
  lazy val e36 = (1 until 1000000).filter(n => isPalindrome(BigInt(n).toString(2)) && isPalindrome(n.toString)).sum

  // Euler 37
  def truncPrime(n: Long) = {
    val s = n.toString
    isPrime(n) && (1 until s.length).forall(i => isPrime(s.substring(i).toLong) && isPrime(s.substring(0, s.length-i).toLong))
  }
  lazy val e37 = primes.filter(n => n > 7 && truncPrime(n)).take(11).sum

  // Euler 38
  def getMults(n: Int) = {
    def multHelper(m: Int, mults: List[Int]): List[Int] = {
      val len: Int = mults.mkString.length
      if (len > 9) List()
      else if (m > 9 || len == 9) mults
      else multHelper(m+1, n*m :: mults)
    }
    multHelper(1, List()).reverse
  }

  lazy val e38 = (10 to 100000).
    map(n => getMults(n).mkString).
    filter(s => s.length == 9 && s.distinct.length == 9 && !s.contains("0")).
    map(_.toInt).max

  // Euler 39
  lazy val e39 = (for { p <- 3 to 1000
                        a <- 1 to (p / 2.0).round.toInt
                        b <- a to (p / 2.0).ceil.toInt
                        c = p - a - b
                        if c > 0 && a * a + b * b == c * c } yield (p, a, b, c)).groupBy(_._1).maxBy(_._2.length)

  // Euler 40
  def digitAtIndex(i: Int): Int =
    if (i < 10) i
    else if (i < 190) ((i - 10) / 2 + 10).toString.charAt((i - 10) % 2).asDigit
    else if (i < 2890) ((i - 190) / 3 + 100).toString.charAt((i - 190) % 3).asDigit
    else if (i < 38890) ((i - 2890) / 4 + 1000).toString.charAt((i - 2890) % 4).asDigit
    else if (i < 488890) ((i - 38890) / 5 + 10000).toString.charAt((i - 38890) % 5).asDigit
    else if (i < 5888890) ((i - 488890) / 6 + 100000).toString.charAt((i - 488890) % 6).asDigit
    else -1

  lazy val e40 = (0 to 6).foldRight(1)((i, mul) => mul * digitAtIndex(math.pow(10, i).toInt))

  // Euler 41
  def isPandigital(num: Long): Boolean = {
    val numStr: String = num.toString
    numStr.length < 10 && "123456789".take(numStr.length).forall(numStr.contains(_))
  }

  lazy val e41 = primes.dropWhile(_ <= 1000000).takeWhile(_ <= 10000000).reverseIterator.find(isPandigital) // 7652413

  // Euler 42
  def isTriangle(t: Long) = ((1 + math.sqrt(1 + 8 * t)) / 2).isWhole() // Reverse of n*(n-1)/2
  lazy val triWords = Source.fromURL(getClass.getResource("p042_words.txt")).mkString.replaceAll("\"", "").split(",")
  lazy val e42 = triWords.map(_.map(_ - 'A' + 1).sum.toLong).count(isTriangle) // 162

  // Euler 43
  lazy val smallPrimes = List(2, 3, 5, 7, 11, 13, 17)
  lazy val e43 = "0123456789".permutations.
    filter(n => (1 to 7).forall(i => n.substring(i, i + 3).toInt % smallPrimes(i-1) == 0)).map(_.toLong).sum // 16695334890

  // Euler 44
  def isPentagonal(p: Long) = ((1 + math.sqrt(1 + 24 * p.toDouble)) / 6).isWhole()

  private val pentagonalLimit: Int = 1000000
  lazy val pentagonals = (1 to pentagonalLimit).map{ case n => (n.toLong*(3*n.toLong-1))/2 }.toArray
  lazy val e44 = (for {j <- 0 until pentagonalLimit
                       k <- j + 1 to j + 1147
                       if k < pentagonalLimit
                       pj = pentagonals(j)
                       pk = pentagonals(k)
                       if isPentagonal(math.abs(pk + pj)) && isPentagonal(math.abs(pk - pj))
  } yield pk - pj) min   // 5482660

  // Euler 45
  def isHexagonal(h: Long) = ((1 + math.sqrt(1 + 8 * h))/4).isWhole()
  lazy val e45 = pentagonals.dropWhile(_ < 40756).find(n => isTriangle(n) && isPentagonal(n) && isHexagonal(n)) // 1533776805

  // Euler 46
  def isGoldbach(g: Long) = !primes35.contains(g) && primes35.exists(p => math.sqrt((g - p)/2).isWhole())
  lazy val e46 = Stream.from(3, 2).find(n => !primes35.contains(n) && !isGoldbach(n)) // 5777

  // Euler 47
  def distinctFactorCount(num: Int, factors: Int) = primeFactors(num).keys.size == factors
  lazy val e47 = Stream.from(1).find(x => List(x, x+1, x+2, x+3).forall(n => distinctFactorCount(n, 4))) // 134043

  // Euler 48
  val digits = 10
  val mod = math.pow(10, digits).toLong
  def modPower(x: Long, pow: Long, modBy: Long, res: Long): Long = pow match {
    case 0 => res
    case p => modPower(x, p-1, modBy, (res * x) % modBy)
  }
  def selfPowers(n: Int) = (1 to n).map(x => modPower(x, x, mod, 1)).sum % mod
  lazy val e48 = selfPowers(1000) // 9110846700

  // Euler 49
  lazy val primes49 = primes.dropWhile(_ < 1000).takeWhile(_ < 10000)
  lazy val e49 = (for {x <- primes49
                       y <- primes49
                       if x < y && x.toString.sorted.equals(y.toString.sorted)
                       z = 2 * y - x
                       if primes49.contains(z) && x.toString.sorted.equals(z.toString.sorted)
  } yield List(x, y, z)).toList.map{_.mkString("")} // List(148748178147, 296962999629)

  // Euler 50
  // Iterative implementation for efficiency. Previous implementations took way too long.
  def maxConsecutivePrimeSum(limit: Int) = {
    val primes50 = primes.takeWhile(_ < limit).toArray
    val primes50Set: HashSet[Long] = HashSet[Long]() ++ primes50
    var sum = 0L
    var max = (0, 0, 0, 0L)
    for (x <- primes50.indices) {
      sum = 0
      for (y <- Range(x, primes50.length)) {
        sum += primes50(y)
        if (primes50Set(sum) && (y-x + 1) > max._1) max = (y-x + 1, x, y, sum)
      }
    }
    max
  }
  lazy val e50 = maxConsecutivePrimeSum(1000000) // (543,3,545,997651)

  // Euler 51
  // Can be more efficient
  def findPrimesFromPattern(p: String) = (0 to 9).map(d => p.replaceAll("\\*", d.toString)).
    map(_.dropWhile(_ == '0')).filter(s => s.length == p.length && isPrime(s.toLong))
  def generatePatternsOfLength(length: Int, wildcards: Int) = {
    val digits = (0 until math.pow(10, length-wildcards).toInt).map(_.toString.sorted).distinct
    val patterns = digits.map(_ + List.fill(wildcards)("*").mkString("")).flatMap(_.permutations).distinct
    patterns
  }
  lazy val e51 = generatePatternsOfLength(6, 3).map(findPrimesFromPattern).find(_.length == 8).get
  // Vector(121313, 222323, 323333, 424343, 525353, 626363, 828383, 929393)

  // Euler 52
  lazy val e52 = Stream.from(1).find(n => (1 to 6).map(mul => (n*mul).toString.sorted).distinct.length == 1).get // 142857

  // Euler 53
  var pascal = Map[(Int, Int), Int]((0, 0) -> 1)
  def memoPascal(n: Int, k: Int): Int = {
    if (k < 0 || k > n) return 0
    if (!pascal.contains((n, k))) {
      val res = memoPascal(n - 1, k - 1) + memoPascal(n - 1, k)
      pascal += (if (res > 1000000) (n, k) -> 1000001 else (n, k) -> res)
    }
    pascal((n, k))
  }
  lazy val e53 = (for {n <- 1 to 100; k <- 0 to n; if memoPascal(n, k) > 1000000} yield 1).length // 4075

  // Euler 54
  //  lazy val cardValues = Seq("A", "1", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K")
  //  lazy val cardSuites = Seq("H", "D", "C", "S")
  case class Card(value: Char, suite: Char)
  abstract class Hand
  case class HighCard(value: Char) extends Hand
  case class OnePair(value: Char) extends Hand
  case class TwoPair(value1: Char, value2: Char) extends Hand
  case class Three(value: Char) extends Hand
  case class Straight(start: Char, end: Char) extends Hand
  case class Flush(start: Char, end: Char) extends Hand
  case class FullHouse(three: Char, two: Char) extends Hand
  case class Four(value: Char) extends Hand
  case class StraightFlush() extends Hand
  lazy val hands = Seq(HighCard, OnePair, TwoPair, Three, Straight, Flush, FullHouse, Four, StraightFlush)

  lazy val pokerHands = Source.fromURL(getClass.getResource("p054_poker.txt")).
    getLines().map(_.split(" ").map(c => Card(c.charAt(0), c.charAt(1)))).map(arr => (arr.slice(0, 5), arr.slice(5, 10)))
  //  println(pokerHands.map{case (p1, p2) => p1.mkString(" ") + " - " + p2.mkString(" ")}.mkString("\n"))
  // TODO when I feel like thinking about cards more :/

  // Euler 55
  def isLychrel(n: Int) = {
    val iterationLimit = 50  // For numbers below 10,000
    def lychrelRec(num: String, iteration: Int): Boolean = {
      if (iteration > iterationLimit) true
      else {
        val next = (BigInt(num) + BigInt(num.reverse)).toString()
        if (isPalindrome(next)) false
        else lychrelRec(next, iteration + 1)
      }
    }
    lychrelRec(n.toString, 0)
  }
  lazy val e55 = 1 to 10000 count isLychrel // 249

  // Euler 56
  def bigPower(a: Int, b: Int, res: BigInt = BigInt(1)): BigInt = if (b == 0) res else bigPower(a, b-1, res*a)
  lazy val bigPowers = for {a <- 2 to 99
                            b <- 1 to 100
                            num = bigPower(a, b)
  } yield num
  lazy val e56 = bigPowers.map(_.toString().toCharArray.map(_.asDigit).sum).max // 972

  // Euler 57
  def gcd(a: BigInt, b: BigInt): BigInt = if (b.equals(BigInt(0))) a else gcd(b, a % b)

  case class Frac(numer: BigInt, denum: BigInt = 1) {
    require(!denum.equals(BigInt(0)), "Can't divide by 0")
    def reduce = {
      val g = gcd(numer, denum)
      Frac(numer / g, denum / g)
    }
    def /(other: Frac) = Frac(numer * other.denum, denum * other.numer).reduce
    def *(other: Frac) = Frac(numer * other.numer, denum * other.denum).reduce
    def +(other: Frac) = Frac(numer * other.denum + other.numer * denum, denum * other.denum).reduce
    def -(other: Frac) = Frac(numer * other.denum - other.numer * denum, denum * other.denum).reduce
    def inv = Frac(denum, numer)
    override def toString =
      (if ((numer < 0) ^ (denum < 0)) "-" else "") + numer.abs.toString + "/" + denum.abs.toString
  }
  object Frac {
    implicit def int2Frac(n: Int): Frac = Frac(n)
    implicit def long2Frac(n: Long): Frac = Frac(n)
  }

  // Improved to a tailrec solution
  def sqrtSeries(expansion: Int) = {
    def sqrtRec(i: Int, res: Frac): Frac = {
      if (i == 1) res
      else sqrtRec(i-1, (2 + res).inv)
    }

    1 + sqrtRec(expansion, Frac(1, 2))
  }
  lazy val e57 = (1 to 1000).map(sqrtSeries).
    count(f => f.numer.toString().length > f.denum.toString().length) // 153

  // Found a much better solution that doesn't involve re-making the same expressions over and over
  lazy val sqrtStream: Stream[Frac] = (1 + Frac(1, 2)) #:: sqrtStream.map(f => 1 + (1 + f).inv)
  lazy val e57b = sqrtStream.take(1000).
    count(f => f.numer.toString().length > f.denum.toString().length)

  // Euler 58
  def primeTest(n: Int): Boolean = {
    if (n <= 1) return false
    if (n == 2) return true
    if (n % 2 == 0) return false
    if (n < 9) return true
    if (n % 3 == 0) return false

    !Stream.from(5, 6).takeWhile(x => x * x <= n).exists(x => n % x == 0 || n % (x + 2) == 0)
  }
  def primesInLayer(n: Int): Int = {
    val largest = (2 * n + 1) * (2 * n + 1)
    val diff = 2 * n
    Seq(largest - 3*diff, largest - 2*diff, largest - diff, largest).count(primeTest)
  }
  lazy val e58 = {
    var i = 1
    var ps = 0
    var ns = 1
    do {
      ns += 4
      ps += primesInLayer(i)
      i += 1
    } while (ps.toDouble / ns >= 0.1)
    i - 1
  } * 2 + 1 // 26241

  // Euler 59
  lazy val cipherText = Source.fromURL(getClass.getResource("p059_cipher.txt")).mkString
  lazy val encoded = cipherText.split(",")
  lazy val possibles = for {a <- 'a' to 'z'
                            b <- 'a' to 'z'
                            c <- 'a' to 'z'
                            code: Stream[Char] = Stream.continually(List(a, b, c).toStream).flatten
                            count = encoded zip code map {
                              case (text, key) => (text.toInt ^ key.toInt).toChar
                            } mkString "" split " and " length
  } yield (a.toString + b.toString + c.toString, count)

  lazy val code: String = possibles.maxBy(_._2)._1

  lazy val codeStream: Stream[Char] = Stream.continually(code.toStream).flatten
  lazy val decrypted = encoded zip codeStream map {
    case (text, key) => (text.toInt ^ key.toInt).toChar
  } mkString ""

  lazy val e59: Int = (encoded zip codeStream map { case (text, key) => text.toInt ^ key.toInt }).toList.sum // 107359


}
