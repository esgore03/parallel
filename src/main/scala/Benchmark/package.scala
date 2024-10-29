package object Benchmark {
  import kmedianas2D._
  import org.scalameter._
  import plotly._, element._, layout._

  def umbral(npuntos:Int):Int = {
    // Si npuntos= 2^n, entonces el umbral será 2^(n/2)
    math.pow(2, ((math.log(npuntos)/math.log(2))/2).toInt).toInt
  }

  def tiempoDe[T](body: => T) = {
    val timeA1 = config(
      KeyValue(Key.exec.minWarmupRuns -> 20),
      KeyValue(Key.exec.maxWarmupRuns -> 60),
      KeyValue(Key.verbose -> false)
    ) withWarmer(new Warmer.Default) measure (body)
    timeA1
  }

  def tiemposKmedianas(puntos:Seq[Punto], k:Int, eta:Double) = {

    val medianas = inicializarMedianas(k, puntos)
    val tiempoSeq = tiempoDe(kMedianasSeq(puntos, medianas, eta))
    val tiempoPar = tiempoDe(kMedianasPar(puntos, medianas, eta))
    (tiempoSeq,tiempoPar, tiempoSeq.value / tiempoPar.value)
  }

  def probarKmedianasSeq(puntos:Seq[Punto], k:Int, eta:Double) = {
    // Probar lo secuencial
    val puntosSeq = puntos
    val medianasSeq = inicializarMedianas(k, puntosSeq)
    val medianasSeqfin = kMedianasSeq(puntosSeq, medianasSeq, eta)
    val clasifFinalSeq = clasificarSeq(puntosSeq,medianasSeqfin)
    val tiempoSeq = tiempoDe(kMedianasSeq(puntosSeq, medianasSeq, eta))

    // Hacer gráfica de los resultados del proceso secuencial
    val trazosSeq = for {
      (p,pseq) <- clasifFinalSeq
      ejeXseq = for {
        pto <- pseq
      } yield pto.x
      ejeYseq = for {
        pto <- pseq
      } yield pto.y
    } yield Scatter(
      ejeXseq,
      ejeYseq
    ).withMode(ScatterMode(ScatterMode.Markers)).withName(s"Puntos: $p.x" ++ s"$p.y")

    val ejeXMedianasSeq = for {
      p <- medianasSeq
    } yield p.x

    val ejeYMedianasSeq = for {
      p <- medianasSeq
    } yield p.y

    val ejeXMedianasFinSeq = for {
      p <- medianasSeqfin
    } yield p.x

    val ejeYMedianasFinSeq = for {
      p <- medianasSeqfin
    } yield p.y

    val trazo2Seq= Scatter(
      ejeXMedianasSeq,
      ejeYMedianasSeq
    ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas")

    val trazo3Seq= Scatter(
      ejeXMedianasFinSeq,
      ejeYMedianasFinSeq
    ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas Finales")

    val dataSeq = trazo2Seq +:  (trazo3Seq +: trazosSeq.toSeq)

    val layoutSeq = Layout().withTitle("Plotting de puntos al azar y medianas iniciales y finales - Versión Secuencial")


    Plotly.plot("kmedianasSeq.html", dataSeq, layoutSeq)
  }

  def probarKmedianasPar(puntos:Seq[Punto], k:Int, eta:Double) = {
    // Probar lo paralelo
    val puntosPar = puntos
    val medianasPar = inicializarMedianas(k, puntosPar)
    val medianasParfin = kMedianasPar(puntosPar, medianasPar, eta)
    val clasifFinalPar = clasificarPar(umbral(puntosPar.length))(puntosPar,medianasParfin)
    val tiempoPar = tiempoDe(kMedianasPar(puntosPar, medianasPar, eta))

    // Hacer gráfica de los resultados del proceso paralelo
    val trazosPar = for {
      (p,ppar) <- clasifFinalPar
      ejeXpar = for {
        pto <- ppar
      } yield pto.x
      ejeYpar = for {
        pto <- ppar
      } yield pto.y
    } yield Scatter(
      ejeXpar.toSeq,
      ejeYpar.toSeq
    ).withMode(ScatterMode(ScatterMode.Markers)).withName(s"Puntos: $p.x" ++ s"$p.y")

    val ejeXMedianasPar = for {
      p <- medianasPar
    } yield p.x

    val ejeYMedianasPar = for {
      p <- medianasPar
    } yield p.y

    val ejeXMedianasFinPar = for {
      p <- medianasParfin
    } yield p.x

    val ejeYMedianasFinPar = for {
      p <- medianasParfin
    } yield p.y

    val trazo2Par= Scatter(
      ejeXMedianasPar,
      ejeYMedianasPar
    ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas")

    val trazo3Par= Scatter(
      ejeXMedianasFinPar,
      ejeYMedianasFinPar
    ).withMode(ScatterMode(ScatterMode.Markers)).withName("Medianas Finales")

    val dataPar = (trazo2Par +:  (trazo3Par +: trazosPar.toSeq))

    val layoutPar = Layout().withTitle("Plotting de puntos al azar y medianas iniciales y finales - Versión Paralela")


    Plotly.plot("kmedianasPar.html", dataPar.toSeq, layoutPar)
  }
}

//  (medianasSeq, medianasSeqfin, clasifFinalSeq, tiempoSeq, tiempoSeq.value)
//  (medianasPar, medianasParfin, clasifFinalPar, tiempoPar, tiempoPar.value)
