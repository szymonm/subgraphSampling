


import pl.szymonmatejczyk.myerson.MyersonApproximationComparison._
import purecsv.safe._


import com.quantifind.charts.Highcharts._



def algo1(results: Seq[Result]) = results
  .filter(_.method == "SamplingCoalitionsMyersonValue").filter(_.graph=="ER(0.4)")
  .map(x => (x.samples, x.error))

def algo2(results: Seq[Result]) = results
  .filter(_.method == "HybridCoalitionsMyersonValue").filter(_.graph=="ER(0.4)")
  .map(x => (x.samples, x.error)).filter(x => x._2 < 1000)

def algo3(results: Seq[Result]) = results
  .filter(_.method == "SumCCBasedSamplingCoalitionsMyersonValue").filter(_.graph=="ER(0.4)")
  .map(x => (x.samples, x.error))

val results = compute()

line(algo1(results))
hold
line(algo2(results))
line(algo3(results))
yAxis("Error")
xAxis("Iterations")

legend(Seq("Algorithm 1", "Algorithm 2 (hybrid)", "Algorithm 3"))
