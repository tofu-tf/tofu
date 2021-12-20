package tofu

/** a collection of classic monomorphic optics based on http://hackage.haskell.org/package/lens and
  * http://julien-truffaut.github.io/Monocle/ using names readable for user unfamiliar with then capable for using as
  * implicit evidences in effect transmogrification
  *
  * see hierarchy here: https://wiki.tcsbank.ru/display/API/optics
  */
package object optics {
  type Same[A, B]       = PSame[A, A, B, B]
  type Equivalent[A, B] = PEquivalent[A, A, B, B]
  type Subset[A, B]     = PSubset[A, A, B, B]
  type Contains[A, B]   = PContains[A, A, B, B]
  type Property[A, B]   = PProperty[A, A, B, B]
  type Repeated[A, B]   = PRepeated[A, A, B, B]
  type Items[A, B]      = PItems[A, A, B, B]
  type Reduced[A, B]    = PReduced[A, A, B, B]
  type Downcast[A, B]   = PDowncast[A, A, B, B]
  type Upcast[A, B]     = PUpcast[A, A, B, B]
  type Extract[A, B]    = PExtract[A, A, B, B]
  type Folded[A, B]     = PFolded[A, A, B, B]
  type Update[A, B]     = PUpdate[A, A, B, B]
  type Zipping[A, B]    = PZipping[A, A, B, B]

  /** label provider for instance discrimination like Contains[A, B] with Label["first"]
    */
  type Label[label] = Any {
    type Label = label
  }
}
