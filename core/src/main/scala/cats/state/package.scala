package cats

package object state {
  type State[S, A] = StateT[Eval, S, A]
  object State extends StateFunctions
}
