import Benchmark._
import kmedianas2D._

val puntos3000_10 = generarPuntos(10, 3000).toSeq
probarKmedianasPar(puntos3000_10, 10, 0.01)
probarKmedianasSeq(puntos3000_10, 10, 0.01)

val p = generarPuntos(10, 3000).toSeq
val m = inicializarMedianas(10, p)
val clasifSeq = clasificarSeq(p,m)
val clasifPar = clasificarSeq(p,m)
clasifSeq.size
clasifPar.size
val resSeq = kMedianasSeq(p, m, 0.01)
val resPar = kMedianasPar(p, m, 0.01)
resSeq.length
resPar.length