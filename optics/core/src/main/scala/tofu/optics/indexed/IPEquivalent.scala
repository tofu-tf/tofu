package tofu.optics
package indexed

trait IPEquivalent[+I, -S, +T, +A, -B]
  extends PEquivalent[S, T, A, B] with IPContains[I, S, T, A, B] with IPSubset[I, S, T, A, B]
