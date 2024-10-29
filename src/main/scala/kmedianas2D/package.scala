  package object kmedianas2D {
    import scala.annotation.tailrec
    import scala.collection.{Map, Seq}
    import scala.collection.parallel.CollectionConverters._
    import scala.util.Random
    import common._

    // Definiciones comunes para las dos versiones (secuencial y paralela)
    class Punto(val x: Double, val y: Double) {
      private def cuadrado(v: Double): Double = v*v
      def distanciaAlCuadrado(that: Punto): Double = cuadrado(that.x-x) + cuadrado(that.y-y)
      private def round(v: Double): Double = (v*100).toInt/100.0

      override def toString: String = s"(${round(x)}, ${round(y)})"
    }
    def generarPuntos(k: Int, num: Int): Seq[Punto] = {
      val randx = new Random(1)
      val randy = new Random(3)
      (0 until num).map({ i =>
        val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
        val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
        new Punto(x, y)
      })
    }
    def inicializarMedianas(k: Int, puntos: Seq[Punto]): Seq[Punto] = {
      val rand = new Random(7)
      val medianasSet = scala.collection.mutable.Set[Punto]()

      while (medianasSet.size < k) {
        medianasSet.add(puntos(rand.nextInt(puntos.length)))
      }

      medianasSet.toSeq
    }

    // Clasificar Puntos
    def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]): Punto = {
      assert(medianas.nonEmpty)
      medianas.map(pto => (pto, p.distanciaAlCuadrado(pto))).sortWith((a, b) => (a._2 < b._2)).head._1
    }

    // Versiones secuenciales
    def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
      if (puntos.isEmpty) medianaVieja
      else {
        new Punto(puntos.map(p => p.x).sum / puntos.length, puntos.map(p => p.y).sum / puntos.length)
      }
    }
    def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
      puntos.groupBy(punto => hallarPuntoMasCercano(punto, medianas))
    }
    def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
      medianasViejas.map{ mediana =>
        val puntos = clasif.getOrElse(mediana, Seq.empty)
        calculePromedioSeq(mediana, puntos)
      }
    }
    def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
      val etaCuadrado = eta*eta
      val pares = medianasViejas.zip(medianasNuevas)
      pares.forall{case (medianaVieja, medianaNueva) => medianaVieja.distanciaAlCuadrado(medianaNueva) < etaCuadrado}
    }
    @tailrec
    final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
      val clasif = clasificarSeq(puntos, medianas)
      val medianasNuevas = actualizarSeq(clasif, medianas)
      if(hayConvergenciaSeq(eta, medianas, medianasNuevas)) medianasNuevas else {
        kMedianasSeq(puntos, medianasNuevas, eta)
      }
    }

    // Versiones paralelas
    def calculePromedioPar(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
      if (puntos.isEmpty) medianaVieja
      else {
        val puntosPar = puntos.par
        new Punto(puntosPar.map(p=>p.x).sum / puntos.length, puntosPar.map(p=>p.y).sum / puntos.length)
      }
    }
    def clasificarPar(umb: Int)(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
      def combinarMaps[K, V](a: Map[K, Seq[V]], b: Map[K, Seq[V]]): Map[K, Seq[V]] = {
        a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Seq.empty)) }
      }

      if(puntos.size > umb) {
        val (left, right) = puntos.splitAt(puntos.length / 2)

        val (res1, res2) = parallel(
          clasificarPar(umb)(left, medianas),
          clasificarPar(umb)(right, medianas))

        combinarMaps(res1, res2)
      }
      else {
        puntos.groupBy(punto => hallarPuntoMasCercano(punto, medianas))
      }
    }
    def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
      medianasViejas.par.map { mediana =>
        val puntos = clasif.getOrElse(mediana, Seq.empty)
        calculePromedioPar(mediana, puntos)
      }.toList
    }
    def hayConvergenciaPar(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
      def umbral(numMedianas: Int): Int = {
        if (numMedianas < 20) 1
        else if (numMedianas < 100) 10
        else if (numMedianas < 500) 25
        else if (numMedianas < 1000) 50
        else 100
      }

      if(medianasViejas.length > umbral(medianasViejas.size)){
        val (medianasViejasLeft, medianasViejasRight) = medianasViejas.splitAt(medianasViejas.length/2)
        val (medianasNuevasLeft, medianasNuevasRight) = medianasViejas.splitAt(medianasViejas.length/2)

        val (res1, res2) = parallel(hayConvergenciaPar(
          eta, medianasViejasLeft, medianasNuevasLeft),
          hayConvergenciaPar(eta, medianasViejasRight, medianasNuevasRight)
        )

        res1 && res2
      }
      else{
        val etaCuadrado = eta*eta
        val pares = medianasViejas.zip(medianasNuevas)
        pares.forall{case (medianaVieja, medianaNueva) => medianaVieja.distanciaAlCuadrado(medianaNueva) < etaCuadrado}
      }
    }
    @tailrec
    final def kMedianasPar(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
      def umbral(npuntos: Int): Int = {
        math.pow(2, ((math.log(npuntos) / math.log(2)) / 2).toInt).toInt
      }

      val clasif = clasificarPar(umbral(puntos.size))(puntos, medianas)
      val medianasNuevas = actualizarPar(clasif, medianas)

      if (hayConvergenciaPar(eta, medianas, medianasNuevas)) medianasNuevas else {
        kMedianasPar(puntos, medianasNuevas, eta)
      }
    }
  }
