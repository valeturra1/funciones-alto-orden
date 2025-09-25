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

val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)


// InsertionSort Ascendente
println("InsertionSort Ascendente")
println(iSort_Asc(List(3, 1, 2)))
println(iSort_Asc(List(5, 4, 3, 2, 1)))
println(iSort_Asc(List(1, 2, 3, 4)))
println(iSort_Asc(List(7)))
println(iSort_Asc(List()))

// InsertionSort Descendente
println("InsertionSort Descendente")
println(iSort_Desc(List(3, 1, 2)))
println(iSort_Desc(List(1, 2, 3, 4, 5)))
println(iSort_Desc(List(5, 5, 5)))
println(iSort_Desc(List(10)))
println(iSort_Desc(List()))

// QuickSort Ascendente
println("QuickSort Ascendente")
println(qSort_Asc(List(3, 1, 2)))
println(qSort_Asc(List(9, 7, 5, 3, 1)))
println(qSort_Asc(List(2, 4, 6, 8)))
println(qSort_Asc(List(42)))
println(qSort_Asc(List()))

// QuickSort Descendente
println("QuickSort Descendente")
println(qSort_Desc(List(3, 1, 2)))
println(qSort_Desc(List(1, 2, 3, 4, 5)))
println(qSort_Desc(List(4, 4, 2, 2)))
println(qSort_Desc(List(99)))
println(qSort_Desc(List()))

// Comparar algoritmos
println("Comparación de algoritmos")
comparar(iSort_Asc, qSort_Asc, List(4, 5, 6, 1, 2, 3))
comparar(iSort_Asc, qSort_Asc, List(10, 9, 8, 7))
comparar(iSort_Asc, qSort_Asc, List(1,1,1,1))
comparar(iSort_Asc, qSort_Asc, List())
comparar(iSort_Asc, qSort_Asc, List(100, 50, 200, 150))

iSort_Asc(List(4, 5, 6, 1, 2, 3))
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
