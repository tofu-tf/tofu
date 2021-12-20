package tofu.env.bio

private[bio] trait EnvBioProducts { self: EnvBio.type =>
  def map2[A, B, C, R, E, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B])(
      f: (A, B) => C
  ): EnvBio[R1, E1, C] =
    ea.map2(eb)(f)

  def map3[A, B, C, D, R, E, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] = ea.map3(eb, ec)(f)

  def parMap2[A, B, C, R, E, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B])(
      f: (A, B) => C
  ): EnvBio[R1, E1, C] = ea.parMap2(eb)(f)

  def parMap3[A, B, C, D, R, E, R1 <: R, E1 >: E](ea: EnvBio[R, E, A], eb: EnvBio[R1, E1, B], ec: EnvBio[R1, E1, C])(
      f: (A, B, C) => D
  ): EnvBio[R1, E1, D] = ea.parMap3(eb, ec)(f)
}
