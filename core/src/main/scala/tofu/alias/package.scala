package tofu

package object alias extends DoSyntaxExtension {
  type Do[F[_]] = DoMonad[F]
}
