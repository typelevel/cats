# Arrow Choice

API Documentation: @:api(cats.arrow.Choice)

Usually we deal with function more often, we're so familiar with `A => B`.

If we have two functions `A => C` and `B => C`, how can we compose them into a single function that can take either A or B and produce a C?

So basically we just look for a function that has type `(A => C) => (B => C) => (Either[A, B] => C)`.

This is exactly typeclass `Choice` provided, if we make `=>` more generic such as `F[_,_]`, you will get a `Choice`

```scala
trait Choice[F[_, _]] {
  def choice[A, B, C](fac: F[A, C], fbc: F[B, C]): F[Either[A, B], C]
}
```

Note the **infix** notation of `choice` is `|||`.

## Middleware
A very useful case of `Choice` is middleware in HTTP server.

Take Http4s for example:

`HttpRoutes[F]` in Http4s is defined as [Kleisli](../datatypes/kleisli.md)

```scala
type HttpRoutes[F[_]] = Kleisli[OptionT[F, *], Request[F], Response[F]]
// defined type HttpRoutes
def routes[F[_]]: HttpRoutes[F] = ???
// defined function routes
```

If we like to have an authentication middleware that composes the route, we can simply define middleware as:

```scala
type Middleware[F[_]] = Kleisli[OptionT[F, *], Request[F], Either[Response[F], Request[F]]]
// defined type Middleware
def auth[F[_]]: Middleware[F] = ???
// defined function auth
```

Which means the `Request[F]` goes through the middleware, will become option of `Either[Response[F], Request[F]]`, where `Left` means the request is denied and return immediately, `Right` means the authentication is OK and request will get pass.

Now we need to define what we should do when middleware returns `Left`:

```scala
def reject[F[_]:Monad]: Kleisli[OptionT[F, *], Response[F], Response[F]] = Kleisli.ask[OptionT[F, *], Response[F]]
// defined function reject
```

Now compose middleware with route

```scala
def authedRoute[F[_]:Monad] = auth[F] andThen (reject[F] ||| routes[F])
// defined function authedRoute
```

You will then get a new route that has authentication ability by composing Kleisli.

## HTTP Response

Another example will be HTTP response handler.

```scala
val resp: IO[Either[Throwable, String]] = httpClient.expect[String](uri"https://google.com/").attempt
```

`attempt` is syntax from [`MonadError`](applicativemonaderror.md)

When we need to handle error, without `Choice` the handler would be something like:
```scala
resp.flatMap{
  case Left => ???
  case Right => ???
}
```

With `Choice` there will be more composable solution without embedded logic in pattern matching:

```scala
def recover[A](error: Throwable): IO[A] = ???
def processResp[A](resp: String): IO[A] = ???

resp >>= (recover _ ||| processResp _)
```

# `ArrowChoice`

`ArrowChoice` is an extended version of `Choice`, which has one more method `choose`, with syntax `+++`

```scala
trait ArrowChoice[F[_, _]] extends Choice[F] {
  def choose[A, B, C, D](f: F[A, C])(g: F[B, D]): F[Either[A, B], Either[C, D]]
}
```

With the middleware example, you can think `ArrowChoice` is middleware of middleware.

For example if we want to append log to middleware of `auth`, that can log both when rejected response and pass request:

```scala
def logReject[F[_]]: Kleisli[OptionT[F, *], Response[F], Response[F]] = ???
// defined function logReject
def logThrough[F[_]]: Kleisli[OptionT[F, *], Request[F], Request[F]] = ???
// defined function logThrough
```

See how easy to compose log functionality into our `authedRoute`:

```scala
def authedRoute[F[_]:Monad] = auth[F] andThen (logReject[F] +++ logThrough[F]) andThen (reject[F] ||| routes[F])
// defined function authedRoute
```
