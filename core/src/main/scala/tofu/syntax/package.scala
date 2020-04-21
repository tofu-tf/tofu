package tofu

package object syntax extends DoSyntaxExtension {
  type Do[F[_]] = DoMonad[F]
}
