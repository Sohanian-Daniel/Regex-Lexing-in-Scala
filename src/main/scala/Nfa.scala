class Nfa[A](var map:Map[A, Map[Char, Set[A]]], var stateCount:Int, var initialState:A, var finalState:Set[A]) {
  // The following methods are only the methods directly called by the test suite. You can (and should) define more.
  def map[B](f: A => B) : Nfa[B] = {
    new Nfa(for ((k, v) <- this.map) yield f(k) -> ( for ((k1, set) <- v) yield (k1, set.map(f))), stateCount, f(initialState), finalState.map(f))
  }

  def next(state:A, c: Char): Set[A] = {
    if (map.contains(state)) {
      map(state).getOrElse(c, Set())

    } else {
      Set()
    }
  }

  def accepts(str: String): Boolean = {
    def aux(str: List[Char], currentState: A): Boolean = {
      str match {
        case x :: xs =>
          var result = false
          for (state <- this.next(currentState, 0.toChar))
            result = result || aux(x::xs, state)
          for (state <- this.next(currentState, x))
            result = result || aux(xs, state)
          result
        case Nil =>
          var result = false
          for (state <- this.next(currentState, 0.toChar))
            result = result || aux(Nil, state)
          result || isFinal(currentState)
      }
    }
    aux(str.toList, initialState)
  }

  def getStates : Set[A] = {
    var result : Set[A] = Set()
    for ((k, v) <- map) {
      result = result + k
      for ((_, set) <- v) {
        result = result ++ set
      }
    }
    result
  }

  def isFinal(state: A): Boolean = {
    finalState.contains(state)
  }

  def getAlphabet : Set[Char] = {
    var result : Set[Char] = Set()
    for ((_, v) <- map) {
      for ((char, _) <- v) {
        if(char != 0.toChar) result = result.+(char)
      }
    }
    result
  }



  // This function creates a new NFA with a transition from the initial state of this nfa to the initial state of the nfa given
  // Modifying the states of the given NFA by mapping it with the function f (usually done to offset the states so that they dont overlap)
  def append(nfa: Nfa[A])(f: A => A): Nfa[A] = {

    def addTransition(nfa:Nfa[A], from: A, to: A, on: Char) = {
      val insideMap : Map[Char, Set[A]] = nfa.map.getOrElse(from, Map())
      nfa.map.updated(from, insideMap + (on -> insideMap.getOrElse(on, Set()).+(to) ))
    }

    def combineMapsOfIterables[K, V](a: Map[K, Map[Char, V]], b: Map[K, Map[Char, V]]): Map[K, Map[Char, V]] = {
      a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Iterable.empty)) }
    }

    val newNfa = nfa.map(f)
    // this is all wrong, it doesnt update the maps and the addTransition is messed up cuz it makes a new thing without fixing stuff around
    val newMap = addTransition(this, initialState, newNfa.initialState, 0.toChar)

    val finalMap : Map[A, Map[Char, Set[A]]] = combineMapsOfIterables(newMap, newNfa.map)//newMap ++ newNfa.map { case (k, v) => k -> (v ++ newMap.getOrElse(k, Iterable.empty)) }


    new Nfa[A](finalMap, this.stateCount + nfa.stateCount, this.initialState, this.finalState ++ newNfa.finalState)
  }

  override def toString: String = {
    val mapFormat = new StringBuilder()
    for ( (from, innerMap) <- this.map) {
      for ( (chr, states) <- innerMap) {
        mapFormat.append("\t\t" + from + " -> ('" + chr + "') -> " + states.toString() + ";\n")
      }
    }

    "NFA[" + this.hashCode() + "] : {\n" +
      "\tTransitions: {\n" +
      mapFormat.toString().replaceAll("'\n'", "\\\\n").split("\n").sortBy(_.split("->")(0).strip().toInt).mkString("\n") +
    "\n\t}\n" +
    "\tInitialStates:" + this.initialState + ";\n" +
      "\tFinalStates:" + this.finalState + ";\n" +
    "\tStateCount:" + this.stateCount + ";\n}"
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {

  // TODO implement Prenex -> Nfa transformation.
  def fromPrenex(str: String): Nfa[Int] = {
    // Get AST from string and unravel PLUS and MAYBE
    val ast = AST(str).unravel()

    var map: Map[Int, Map[Char, Set[Int]]] = Map().withDefaultValue(Map())
    var state_count : Int = 0
    var final_state : Int = 0

    def addStateEpsilon(): Unit = {
      map = map + (state_count -> Map(0.toChar -> Set(state_count + 1)).withDefaultValue(Set()))
      state_count += 1
    }

    def toMap(ast: AST): Unit = {
      ast match {
        case ATOM(a) =>
          map = map + (state_count -> Map(a -> Set(state_count + 1)).withDefaultValue(Set()))
          state_count += 1
          final_state = state_count;
        case EPSILON =>
          addStateEpsilon()
          final_state = state_count;
        case CONCAT(left, right) =>
          // Create map for left
          toMap(left)

          // Add eps transition to next automaton
          addStateEpsilon()

          // Create right automaton
          toMap(right)

          final_state = state_count;
        case UNION(left, right) =>
          // Add empty state with eps transition to first
          val anchor_state = state_count

          addStateEpsilon()

          // Create map for left
          toMap(left)

          val left_final = state_count

          // Add a transition from anchor state to a new automaton
          map = map + (anchor_state -> Map(0.toChar -> Set(state_count + 1, anchor_state + 1)).withDefaultValue(Set()))
          state_count += 1

          // Make automaton for right
          toMap(right)

          final_state = state_count + 1

          // Connect the two automatons to a final state
          map = map + (state_count -> Map(0.toChar -> Set(final_state)).withDefaultValue(Set()))
          map = map + (left_final -> Map(0.toChar -> Set(final_state)).withDefaultValue(Set()))

          state_count += 1;
        case STAR(x) =>
          val starBeginning = state_count

          // Add epsilon transition to STAR start state
          addStateEpsilon()

          val automatonBeginning = state_count
          toMap(x)

          // Add epsilon transition from currentState (final of automaton) to the beginning of it
          // And to a final state
          map = map + (state_count -> Map(0.toChar -> Set(automatonBeginning, state_count + 1 )))

          // Add epsilon transition from initial to the now final state
          map = map + (starBeginning -> Map(0.toChar -> Set(state_count + 1, starBeginning + 1)))
          state_count += 1
          final_state = state_count
        case EMPTY =>
          map = Map().withDefaultValue(Map())
          final_state = 1
          state_count = 1
      }
    }
    toMap(ast)
    new Nfa[Int](map, state_count + 1, 0, Set(final_state))
  }

  def empty(): Nfa[Int] = {
    new Nfa[Int](Map[Int, Map[Char, Set[Int]]](0 -> Map()), 1, 0, Set.empty)
  }
}