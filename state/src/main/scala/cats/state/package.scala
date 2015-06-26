package cats

import free.Trampoline

package object state {
  type State[S, A] = StateT[Trampoline, S, A]
  object State extends StateFunctions
}
