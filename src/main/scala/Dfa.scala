import scala.collection.immutable.ListMap

class Dfa[A] (map:Map[A, Map[Char, A]], var stateCount:Int, var initialState:A, var finalStates:Set[A], var sinkState:A){

  def map[B](f: A => B) : Dfa[B] = {
    new Dfa(for ((k, v) <- this.map) yield f(k) -> ( for ((k1, v1) <- v) yield (k1, f(v1))), stateCount, f(initialState), finalStates.map(f), f(sinkState))
  }

  def next(state:A, c: Char): A = {
    if (map.contains(state)) {
      map(state).getOrElse(c, sinkState)
    } else {
      sinkState
    }
  }

  def accepts(str: String): Boolean = {
    def aux(str: List[Char], currentState: A): Boolean = {
      if (currentState == sinkState) return false
      str match {
        case x :: xs =>
          var result = false
          result = result || aux(xs, this.next(currentState, x))
          result
        case Nil =>
          isFinal(currentState)
      }
    }
    aux(str.toList, initialState)
  }

  // Returns: (read_part, remaining, lastFinalState)
  def read(str: String, prog: Int = 0, line: Int = 0): (String, String, A, Int, Int) = {
    var currentState = initialState
    val read = new StringBuilder()
    val remaining = new StringBuilder(str)
    var finalState: A = initialState

    var longestAcceptedWord = new String()
    var longestAcceptedRemaining = new String()

    var i = prog
    var ln = line

    currentState = this.next(currentState, remaining.head)
    while (currentState != sinkState && remaining.nonEmpty) {
      // Get next state and consume 1 character
      if (remaining.head == '\n') {
        i = 0
        ln = ln + 1
      }
      read += remaining.head
      i = i + 1
      remaining.deleteCharAt(0)

      if (isFinal(currentState)) {
        finalState = currentState
        longestAcceptedWord = read.toString()
        longestAcceptedRemaining = remaining.toString()
      }

      if (remaining.nonEmpty) currentState = this.next(currentState, remaining.head)

    }
    if (remaining.isEmpty) i = str.length
    if (read.length > longestAcceptedWord.length && finalState != initialState) i = i - (read.length - longestAcceptedWord.length)
    (longestAcceptedWord, longestAcceptedRemaining, finalState, i, ln)
  }

  def getStates : Set[A] = {
    var result : Set[A] = Set()
    for ((k, v) <- map) {
      result = result + k
      for ((_, set) <- v) {
        result = result + set
      }
    }
    result
  }

  def isFinal(state: A): Boolean = {
    finalStates.contains(state)
  }

  def reverse(): Nfa[A] = {
    // sinkState becomes a new initial state
    var map :Map[A, Map[Char, Set[A]]] = Map()

    for ((k, v) <- this.map) {

      for ((char, state) <- v) {
        var innerMap: Map[Char, Set[A]] = Map()
        if (map.contains(state)) {
          innerMap = map.apply(state)
        }

        if(innerMap.contains(char)) {
          innerMap = innerMap + (char -> innerMap.apply(char).+(k))
        } else {
          innerMap = innerMap + (char -> Set(k))
        }
        map = map + (state -> innerMap)
      }
    }
    map = map + (sinkState -> Map(0.toChar -> this.finalStates))
    new Nfa[A](map, this.stateCount + 1, sinkState, Set(this.initialState))
  }

  override def toString: String = {
    val mapFormat = new StringBuilder()
    for ( (from, innerMap) <- this.map) {
      for ( (chr, states) <- innerMap) {
        mapFormat.append("\t\t" + from + "-> (" + chr + ") -> " + states + ";\n")
      }
    }

    "DFA[" + this.hashCode() + "] : {\n" +
      "\tTransitions: {\n" +
      mapFormat.toString().replaceAll("\\(\n\\)|'\n'", "\\\\n").split("\n").sortBy(_.split("->")(0).strip().toInt).mkString("\n") +
      "\n\t}\n" +
      "\tInitialStates:" + this.initialState + ";\n" +
      "\tFinalStates:" + this.finalStates + ";\n" +
      "\tStateCount:" + this.stateCount + ";\n}"
  }
}

object Dfa {
  def EpsilonClosure(nfa: Nfa[Int], start: Int) : Set[Int] = {
    def aux(nfa: Nfa[Int], current:Int, visited: Set[Int]): Set[Int] = {
      var set = nfa.next(current, 0.toChar)
      for (state <- set) {
        if (visited.contains(state)) return set
        set = set.++(aux(nfa, state, visited.+(state)))
      }
      set
    }
    aux(nfa, start, Set())
  }

  def fromDfa(nfa: Nfa[Int]): Dfa[Int] = {
    // Map (stateGroupId, ( (states it represents in NFA), (has it been visited) )
    var finalStates : Set[Int] = Set()
    var stateGroups : Map[Int, (Set[Int], Boolean)] = Map()

    var state_count = 0
    stateGroups = stateGroups + (0 -> (EpsilonClosure(nfa, nfa.initialState).+(0), false))
    state_count = 0
    var map:Map[Int, Map[Char, Int]] = Map()

    def findStateGroups(stateGroupId: Int): Unit = {
      // For every char in alphabet
      for (atom <- nfa.getAlphabet) {
        // For every state that is part of our stateGroup

        var totalStates: Set[Int] = Set()
        for (directState <- stateGroups(stateGroupId)._1) {
          // If there is a final state in the group of states, the entire group should be final
          if (nfa.isFinal(directState)) finalStates = finalStates.+(stateGroupId)
          // Calculate all possible states we can visit from this state on this character
          var possibleStates = nfa.next(directState, atom)

          // Get Epsilon Closures
          for (closure <- possibleStates) {
            possibleStates = possibleStates.++(EpsilonClosure(nfa, closure))
          }

          // concatenate all possible states here
          totalStates = totalStates.++(possibleStates)
        }

          // Check if we have any transitions to not waste time
          if (totalStates.nonEmpty) {
            // Check if possibleStates is a known state group
            var isContained = false
            for ((k, set) <- stateGroups) {

              if (totalStates == set._1) {
                isContained = true
                // Add transition from current state group to this stateGroup that it transitions on this atom
                val innerMap = map.getOrElse(stateGroupId, Map((atom, k))).updated(atom, k)
                map = map + (stateGroupId -> innerMap)
              }
            }

            if (!isContained) {
              // Else if we didnt find any set, we make a new state and stateGroup
              val innerMap = map.getOrElse(stateGroupId, Map((atom, state_count + 1))).updated(atom, state_count + 1)
              map = map + (stateGroupId -> innerMap)
              stateGroups = stateGroups + (state_count + 1 -> (totalStates, false))
              state_count += 1
            }
          }
        }
      // Mark stateGroup as being visited
      stateGroups = stateGroups + (stateGroupId -> (stateGroups(stateGroupId)._1, true))
    }

    def allVisited(): Boolean = {
      for ((_, v) <- stateGroups) {
        if (!v._2) return false
      }
      true
    }

    // This sorta reminds me of djikstra
    var i: Int = 0
    while (!allVisited()) {
      findStateGroups(i)
      i += 1
    }
    new Dfa(map, state_count + 1, 0, finalStates, state_count + 2)
  }

  def fromPrenex(str: String): Dfa[Int] = {
    val nfa: Nfa[Int] = Nfa.fromPrenex(str)

    val dfa = fromDfa(nfa)

    // Special case
    if (dfa.finalStates.isEmpty && str == "eps") dfa.finalStates = Set(0)
    dfa
  }

  // Function makes a DFA and keeps track of the final states and the lexeme it represents
  def fromLexNfa(nfa: Nfa[Int], finalStateMapping: ListMap[String, Set[Int]]): (Dfa[Int], ListMap[String, Set[Int]]) = {
    // Map (stateGroupId, ( (states it represents in NFA), (has it been visited) )
    var finalStates : Set[Int] = Set()
    var stateGroups : Map[Int, (Set[Int], Boolean)] = Map()
    var dfaStateMapping: ListMap[String, Set[Int]] = ListMap.empty

    var state_count = 0
    stateGroups = stateGroups + (0 -> (EpsilonClosure(nfa, nfa.initialState).+(0), false))
    state_count = 0
    var map:Map[Int, Map[Char, Int]] = Map()

    def findStateGroups(stateGroupId: Int): Unit = {
      // For every char in alphabet
      for (atom <- nfa.getAlphabet) {
        // For every state that is part of our stateGroup

        var totalStates: Set[Int] = Set()
        for (directState <- stateGroups(stateGroupId)._1) {
          // If there is a final state in the group of states, the entire group should be final
          if (nfa.isFinal(directState)) finalStates = finalStates.+(stateGroupId)

          // Check if the stateGroup contains a final state of the lexer
          for ( (lex, states) <- finalStateMapping ) {
            if (states.contains(directState)) {
              // Add a new entry (lex, lexSet + directState)
              dfaStateMapping = dfaStateMapping.updated(lex, dfaStateMapping.getOrElse(lex, Set()) + stateGroupId)
            }
          }

          // Calculate all possible states we can visit from this state on this character
          var possibleStates = nfa.next(directState, atom)

          // Get Epsilon Closures
          for (closure <- possibleStates) {
            possibleStates = possibleStates.++(EpsilonClosure(nfa, closure))
          }

          // concatenate all possible states here
          totalStates = totalStates.++(possibleStates)
        }

        // Check if we have any transitions to not waste time
        if (totalStates.nonEmpty) {
          // Check if possibleStates is a known state group
          var isContained = false
          for ((k, set) <- stateGroups) {

            if (totalStates == set._1) {
              isContained = true
              // Add transition from current state group to this stateGroup that it transitions on this atom
              val innerMap = map.getOrElse(stateGroupId, Map((atom, k))).updated(atom, k)
              map = map + (stateGroupId -> innerMap)
            }
          }

          if (!isContained) {
            // Else if we didnt find any set, we make a new state and stateGroup
            val innerMap = map.getOrElse(stateGroupId, Map((atom, state_count + 1))).updated(atom, state_count + 1)
            map = map + (stateGroupId -> innerMap)
            stateGroups = stateGroups + (state_count + 1 -> (totalStates, false))
            state_count += 1
          }
        }
      }
      // Mark stateGroup as being visited
      stateGroups = stateGroups + (stateGroupId -> (stateGroups(stateGroupId)._1, true))
    }

    def allVisited(): Boolean = {
      for ((_, v) <- stateGroups) {
        if (!v._2) return false
      }
      true
    }

    // This sorta reminds me of djikstra
    var i: Int = 0
    while (!allVisited()) {
      findStateGroups(i)
      i += 1
    }

    (new Dfa(map, state_count + 1, 0, finalStates, state_count + 2), dfaStateMapping)
  }

  def minimise(dfa: Dfa[Int]): Dfa[Int] = {
    fromDfa(fromDfa(dfa.reverse()).reverse())
  }

  /*def minimiseLex(dfa: Dfa[Int], lexMap: Map[String, Set[Int]]): (Dfa[Int], Map[String, Set[Int]]) = {
    val nfa1 = dfa.reverse()
    val (dfa1, tLexMap1) = fromLexNfa(nfa1, lexMap)
    val nfa2 = dfa1.reverse()
    fromLexNfa(nfa2, tLexMap1)
  }*/
}
