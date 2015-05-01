package pl.szymonmatejczyk

/**
 * Created by szymonmatejczyk on 21.04.15.
 */
package object myerson {
  def factorial(num: Int): BigDecimal = {
    (1 to num).map(x => BigDecimal.valueOf(x)).foldLeft(BigDecimal.valueOf(1)) ((a,b) => (a * b))
  }
}
