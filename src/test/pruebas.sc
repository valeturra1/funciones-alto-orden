import Comparador._
import scala.util.Random

val random = new Random()

def listaAlAzar(long: Int): List[Int] = {
  // Crea una lista de long enteros,
  // con valores aleatorios entre 1 y long*2
  val v = Vector.fill(long) {
    random.nextInt(long * 2)+1
  }
  v.toList
}

def menorQue(a: Int, b: Int): Boolean = a < b
def mayorQue(a: Int, b: Int): Boolean = a > b

val iSort_Asc = insertionSort[Int](menorQue)
val iSort_Desc = insertionSort[Int](mayorQue)

iSort_Asc(List(4, 5, 6, 1, 2, 3))

val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)

qSort_Asc(List(4, 5, 6, 1, 2, 3))

comparar(iSort_Asc, qSort_Asc, List(4, 5, 6, 1, 2, 3))
comparar(iSort_Asc, qSort_Desc, List(4, 5, 6, 1, 2, 3))

val lAsc100 = (1 to 100).toList
val lAsc1000 = (1 to 1000).toList
val lDsc100 = (100 to 1 by -1).toList
val lDsc1000 = (1000 to 1 by -1).toList

comparar(iSort_Asc, qSort_Asc, lAsc100)
comparar(iSort_Asc, qSort_Asc, lAsc1000)
comparar(iSort_Asc, qSort_Asc, lDsc100)
comparar(iSort_Asc, qSort_Asc, lDsc1000)

val l5 = listaAlAzar(5)
val l10 = listaAlAzar(10)
val l20 = listaAlAzar(20)
val l50 = listaAlAzar(50)

iSort_Asc(l5)
iSort_Asc(l10)
iSort_Asc(l20)
iSort_Asc(l50)

iSort_Desc(l5)
iSort_Desc(l10)
iSort_Desc(l20)
iSort_Desc(l50)

qSort_Asc(l5)
qSort_Asc(l10)
qSort_Asc(l20)
qSort_Asc(l50)

qSort_Desc(l5)
qSort_Desc(l10)
qSort_Desc(l20)
qSort_Desc(l50)

comparar(iSort_Asc, qSort_Asc, l5)
comparar(iSort_Asc, qSort_Asc, l10)
comparar(iSort_Asc, qSort_Asc, l20)
comparar(iSort_Asc, qSort_Asc, l50)