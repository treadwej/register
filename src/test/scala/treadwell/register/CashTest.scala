package treadwell.register

import org.scalatest.funsuite.AnyFunSuite

class CashTest extends AnyFunSuite {
  // Helpers for making the tests more expressive and concise.
  private val $0  = Cash()
  private val $1  = $0 + (1 -> 1)
  private val $2  = $0 + (2 -> 1)
  private val $5  = $0 + (5 -> 1)
  private val $10 = $0 + (10 -> 1)

  test("Denominations have consistent identities") {
    assertResult($0) { Cash() }

    // Tests the empty register object and each individual denomination.
    for (denom <- $0 :: $1 :: $2 :: $5 :: Nil) {
      assertResult(denom) { denom + $0 }
      assertResult(denom) { $0 + denom } // tests commutativity
      assertResult(denom) { denom - $0 }
      assertResult(denom) { denom*1 }
      assertResult(denom) { denom*2 - denom }
      assertResult(denom) { denom*(-1) + denom*2 }

      assertResult(denom*2) { denom + denom }
      assertResult(denom*3) { denom + denom + denom }

      assertResult($0) { denom - denom }
      assertResult($0) { denom*0 }
    }
  }

  test("Denominations have consistent values") {
    for ((denom, value) <- ($0, 0) :: ($1, 1) :: ($2, 2) :: ($5, 5) :: Nil) {
      assertResult(value) { denom.total }
      assertResult(value) { (denom*1).total }
      assertResult(value) { ($1*value).total }
      assertResult(value*2) { (denom*2).total }

      assertResult(0) { (denom - denom).total}
      assertResult(value + 10) { (denom + $10).total }
    }
  }

  test("Combinations are consistent and distinct") {
    for ((a, b) <- ($2, $1*2)  :: // compare one $2 note with a pair of $1 notes
                   ($2, $1+$1) :: // as well as the sum of two $1 Cash objects
                   ($5, $1*5)  ::
                   ($5, $2*2 + $1) :: // $5 as a single bill vs as three notes
                   ($2 + $1*3, $2*2 + $1) ::
                   ($5 + ($2 + $1*3), $2*2 + ($1 + $5)) ::
                   (($5 + $2)*2 + $1*3, ($2*2 + $1) + ($5 + $1*7)) :: Nil) {
      assert(a != b)
      assert($0 != a - b) // sums to 0, but contains negative counts

      assertResult(a.total) { b.total }
      assertResult(a.total*2) { (a + b).total }
      assertResult(0) { (a - b).total }
    }
  }

  test("Make change with a single denomination") {
    assertResult($0) { $0 draw 0 }
    assertResult($0) { $0 draw 1 }

    for (denom <- $1 :: $2 :: $5 :: Nil) {
      assertResult(denom) { denom draw denom.total }
      assertResult(denom) { denom*2 draw denom.total }
      assertResult(denom*2) { denom*2 draw (denom.total*2) }
      assertResult($0) { denom draw 0 }

      assertResult($0) { denom draw (denom.total+1) }
      assertResult($0) { denom draw (denom.total*2) }
      assertResult($0) { $10 draw denom.total }

      val mult = denom * (10/denom.total)
      assertResult(mult) { mult draw 10 }
      assertResult(mult) { (mult*2) draw 10 }
      assertResult(mult*2) { (mult*2) draw 20 }

      assertResult(denom) { (denom + $10) draw denom.total }
      assertResult($10) { (denom + $10) draw 10 }
    }
  }

  test("Make change with multiple denominations") {
    val combo = $5*3 + $2*2 + $1*2
    assertResult($1) { combo draw 1 }
    assertResult($2) { combo draw 2 }
    assertResult($2*2) { combo draw 4 }
    assertResult($5*1) { combo draw 5 }
    assertResult($5*2) { combo draw 10 }
    assertResult($5*3) { combo draw 15 }
    assertResult(combo) { combo draw combo.total }

    assertResult($2 + $1) { combo draw 3 }
    assertResult($5 + $1) { combo draw 6 }
    assertResult($5 + $2 + $1) { combo draw 8 }
    assertResult(combo - $5*3) { (combo - $5*3) draw 6 }

    assertResult($5 + $2 + $1) { (combo*5000) draw 8 }
    assertResult($5*2 + $2*2 + $1) { (combo - $5) draw 15 }
  }

  test("Draw $8 from $13") {
    val waysToMake8 =
      for (fives <- 0 to 1;
           twos  <- 0 to (8 - 5*fives)/2;
           ones   = 8 - 5*fives - 2*twos)
        yield $5*fives + $2*twos + $1*ones

    for (tens  <- 0 to 1;
         fives <- 0 to (13 - 10*tens)/5;
         twos  <- 0 to (13 - 10*tens - 5*fives)/2;
         ones   = 13 - 10*tens - 5*fives - 2*twos) {
      val combo13 = $10*tens + $5*fives + $2*twos + $1*ones

      /* The sub-combo that sums to 8 and has the fewest items as well as the
         largest denominations possible. */
      val optimal8 = waysToMake8
        .filter { m8 => m8.denoms.forall { d => m8(d) <= combo13(d) } }
        .maxByOption(m8 => m8.denoms.foldLeft(0) {
          (sum, d) => sum + d*d*m8(d)
        })
        .getOrElse($0)

      assertResult(optimal8) { combo13 draw 8 }
    }
  }
}