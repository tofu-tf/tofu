package tofu.optics
package indexed

trait IPSubset[+I, -S, +T, +A, -B]
    extends PSubset[S, T, A, B] with IPProperty[I, S, T, A, B] with IPUpcast[I, S, T, A, B]
