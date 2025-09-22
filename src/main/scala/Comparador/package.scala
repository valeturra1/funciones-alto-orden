package object Comparador {
  type AlgoritmoOrd[T] = List[T] => (List[T], Int)
  type Comparador[T] = (T, T) => Boolean

  def insert[T](e: T, l: List[T], comp: Comparador[T]): (List[T], Int) = {
    // Recibe un elemento e de tipo T a insertar en una lista ordenada l de elementos de tipo T
    // y devuelve, en una pareja la lista ordenada incluyendo el elemento e
    // y cuantas comparaciones se hicieron para lograrlo

    if (l.isEmpty) {
      (e :: Nil, 0)
    } else if (comp(e, l.head)) {
      (e :: l, 1)
    } else {
      val (tailList, newComp) = insert(e, l.tail, comp)
      (l.head :: tailList, newComp + 1)
    }

  }

  def insertionSort[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
    // Recibe una lista de elementos de tipo T y un comparador de esos elementos
    // y devuelve la lista ordenada y el numero de comparaciones realizadas en una pareja
    // usando el InsertionSort

    def insertionSortAux(l: List[T]): (List[T], Int) = {
      if (l.isEmpty) {
        (Nil, 0)
      } else {
        val (sortedTail, numCompAux) = insertionSortAux(l.tail)
        val (result, compInsert) = insert(l.head, sortedTail, comp)
        (result, numCompAux + compInsert)
      }
    }
    insertionSortAux

  }

  def menoresQue_noMenoresQue[T](l: List[T], v: T, comp: Comparador[T]): (List[T], List[T], Int) = {
    // Recibe la una lista de elementos de tipo T y un valor v de tipo T
    // devuelve la lista de elementos de l que son menores que v,
    // la lista de los que no son menores que v
    // y cuantas comparaciones se hicieron para llegar a ella

    def particionAux(faltantes: List[T], menores: List[T], noMenores: List[T], count: Int): (List[T], List[T], Int) = {
      if (faltantes.isEmpty) {
        (menores, noMenores, count)
      } else if (comp(faltantes.head, v)) {
        particionAux(faltantes.tail, menores ++ List(faltantes.head), noMenores, count + 1)
      } else {
        particionAux(faltantes.tail, menores, noMenores ++ List(faltantes.head), count + 1)
      }
    }
    particionAux(l, Nil, Nil, 0)

  }

  def quickSort[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
    // Recibe una lista de elementos de tipo T y un comparador de esos elementos
    // y devuelve la lista ordenada y el numero de comparaciones realizadas en una pareja
    // Usando el quickSort

    def quickSortAux(l: List[T]): (List[T], Int) = {
      if (l.isEmpty || l.tail.isEmpty) {
        (l, 0)
      } else {
        val head = l.head

        val (menores, noMenores, compParticion) = menoresQue_noMenoresQue(l.tail, head, comp)

        val (ordenadoMenores, compMenores) = quickSortAux(menores)
        val (ordenadoNoMenores, compNoMenores) = quickSortAux(noMenores)

        val result = ordenadoMenores ++ (head :: ordenadoNoMenores)
        val compTotal = compParticion + compMenores + compNoMenores

        (result, compTotal)
      }
    }
    quickSortAux

  }

  def comparar[T](a1: AlgoritmoOrd[T], a2: AlgoritmoOrd[T], l: List[T]): (Int, Int) = {
    // Recibe dos algoritmos de ordenamiento y una lista para ordenar
    // y devuelve una pareja con el numero de comparaciones hechas por a1,
    // y el numero de comparaciones hechas por a2 para esa instancia de l en particular
    // si los dos algoritmos dan el mismo resultado
    // sino, devuelve (-1,-1)

    val (l1, c1) = a1(l)
    val (l2, c2) = a2(l)
    if (l1 == l2) (c1, c2) else (-1, -1)
  }
}