---
layout: docs
title:  "StateT"
section: "data"
source: "core/src/main/scala/cats/data/package.scala"
scaladoc: "#cats.data.StateT"
---
# StateT

`StateT[F[_], S, A]` is a datatype used to define state machine effectful computations that can be easily composed into a larger one.

The type parameters are:
- `F[_]` representing the effect in which the computation is performed.
- `S` representing the underlying state, shared between each step of the state machine.
- `A` representing the return value.

It can be seen as a way to add the capability to manipulate a shared state, to an existing computation in the context of `F`.

This definition could be confusing, but it will become clear after learning the `State` monad and by the example below.

## StateT and State Relationship

`StateT` is a monad transformer for [State](https://typelevel.org/cats/datatypes/state.html), in particular [State](https://typelevel.org/cats/datatypes/state.html) is defined as a `StateT` with [Eval](https://typelevel.org/cats/datatypes/eval.html) as the effect `F`.
Therefore, `StateT` exposes the same methods of `State`, such as: `modify`, `get` and `set`.
Plus additional methods, that handles effectful computations, eg: `modifyF`, `setF` and `liftF`.

## Example: Hangman Game

In this example we need to:
- Keep a state of the game, with the target word and the guesses made.
- Show the game state.
- Ask the player to input a guess word until the game is over.

The whole game state machine can be summarized as following:

```asciidoc

    +----------------------+                 +----------------------+                   +-------------------+
    | Ask for Target Word  +---------------->| Show the word hidden |------------------>| Ask for the guess |
    +----------------------+                 +----------------------+                   +-------------------+
                                                    ^                                             |
                                                    |                                             |
                                                    |                                             |
                  +-----------+          +---------------+       +-----------------------+        |
                  | Game Over |<---------+ Check the win |<------| Compute the new state |<-------+
                  +-----------+          |   condition   |       +-----------------------+
                                         +---------------+

```

At each step, the state might be updated and, since some I/O operations needs to be performed along the way, the specific effect (typically `IO` monad, but in our case it will be `Eval`) has to be used.
We can model the state as:

```scala mdoc:silent
import cats.data.StateT
import cats.Eval
import scala.io.StdIn._

final case class GameState(
    target: String,
    guessedChars: Set[Char],
    attemptNum: Int
)

sealed trait GameOver
case object Win extends GameOver
case object Loose extends GameOver

object GameState {

  val maxAttempts: Int = 3

  def apply(target: String): GameState = GameState(
    target = target,
    guessedChars = Set.empty[Char],
    attemptNum = 0
  )

  def showWordHidden(state: GameState): String =
    state.target.map(c => if (state.guessedChars.contains(c)) c else '*')

  def attemptGuess(
      state: GameState,
      guess: String
  ): Either[GameOver, GameState] =
    state match {
      case GameState(t, _, _) if t == guess => Left(Win)
      case GameState(_, _, attemptNum) if attemptNum == (maxAttempts - 1) =>
        Left(Loose)
      case GameState(target, guessedChars, attemptNum) =>
        Right(
          GameState(
            target = target,
            guessedChars = guessedChars ++ guess,
            attemptNum = attemptNum + 1
          )
        )
    }

}
```

In the code above you can see some useful functions, such the one that returns the encrypted target word. A brief example here:

```scala mdoc
val gameState1 = GameState("cats")
println(GameState.showWordHidden(gameState1))
val gameState2 = GameState.attemptGuess(gameState1, "bats")
println(GameState.showWordHidden(gameState2.getOrElse(???)))
```

Let's focus on the state machine game steps.

```scala mdoc:silent
object StateMachine {

  def step1(): Eval[GameState] = for {
    _ <- Eval.later(println("Insert the target word: "))
    word <- Eval.later(readLine())
  } yield GameState(word)

  def step2(): StateT[Eval, GameState, Unit] = for {
    state <- StateT.get[Eval, GameState]
    _ <- StateT.liftF(Eval.later(println(GameState.showWordHidden(state))))
  } yield ()

  def step3(): StateT[Eval, GameState, String] = for {
    _ <- StateT.liftF(Eval.later(println("Insert the guess word: ")))
    guess <- StateT.liftF(Eval.later(readLine()))
  } yield guess

  def step4(
      guess: String
  ): StateT[Eval, GameState, Either[GameOver, GameState]] =
    StateT.get[Eval, GameState].map(state => GameState.attemptGuess(state, guess))

  def step5(
      exitCondition: Either[GameOver, GameState]
  ): StateT[Eval, GameState, Unit] =
    exitCondition match {
      case Left(gameOver) => step6(gameOver)
      case Right(nextState) =>
        for {
          _ <- StateT.set[Eval, GameState](nextState)
          _ <- StateT.liftF(Eval.later(println("Wrong Guess, retry")))
          _ <- hangman()
        } yield ()
    }

  def step6(gameResult: GameOver): StateT[Eval, GameState, Unit] =
    gameResult match {
      case Win =>
        StateT.liftF(Eval.later(println("Congratulations, you won!!!")))
      case Loose => StateT.liftF(Eval.later(println("I'm sorry, you loose!!!")))
    }

  def hangman(): StateT[Eval, GameState, Unit] = for {
    _ <- step2()
    guess <- step3()
    exitCondition <- step4(guess)
    _ <- step5(exitCondition)
  } yield ()
}
```

Here you can see how the state machine is setup. In particular, there's a loop where, if the exit condition is not reached, a new attempt is requested.
Finally, we can wire everithing together in the following way:

```scala mdoc:compile-only
(for {
  initialState <- StateMachine.step1()
  _ <- StateMachine.hangman().run(initialState)
} yield ()).value
```

We hope this example helps clarifying how `StateT` can help in designing a computation based on state machine steps that may requires side effects or other capabilities, eg `Option`, `List`, `Fiber` etc.

You can find the full source of the example [here](https://gist.github.com/benkio/46f5aea4f15ec059f02d6bfe9bd25e99)
