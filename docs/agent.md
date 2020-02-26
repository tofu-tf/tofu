---
 id: agent
 title: Agent
 ---

 ### Agent: Reference with effectful mutators
 
 Agent is like cats-effect Ref but it allows effectful updates of referenced value. 
 It also allows enqueuing of mutations without waiting for their completion 
 and mutation of values by filter(partial function). 
 
 ```scala
 trait Agent[F[_], A] {
 
   def get: F[A]
 
   def updateM(f: A => F[A]): F[A]
 
   def fireUpdateM(f: A => F[A]): F[Unit]
 
   def modifyM[B](f: A => F[(B, A)]): F[B]
 
   def updateSomeM(f: PartialFunction[A, F[A]]): F[A]
 
   def modifySomeM[B](default: B)(f: PartialFunction[A, F[(B, A)]]): F[B]
 }
 ```
 
 Agent's companion object contains default implementation 
 that can be built from cats-effect's Ref and Semaphore:
 
 ```scala
 object Agent {
 
   final case class SemRef[F[_]: Monad: Fire, A](ref: Ref[F, A], sem: Semaphore[F]) extends Agent[F, A] {
   // impl code...
   }
}
 ```
 
 ### Creation
 
 One can create Agent of some value with helper MakeAgent. 
 
 It allows different effects for creating and running Agent:  
 
 ```scala
 trait MakeAgent[I[_], F[_]] {

   def agentOf[A](a: A): I[Agent[F, A]]
 }
 ```
 MakeAgent has a companion object that offers easier creation of Agent instances.
 Agent could also be constructed from tofu.concurrent's MakeRef and MakeSemaphore implicit instances.
```scala
object MakeAgent {
def apply[I[_], F[_]](implicit mkAgent: MakeAgent[I, F]): Applier[I, F] = // impl

  implicit def byRefAndSemaphore[I[_]: FlatMap, F[_]: Monad: Fire](
      implicit
      refs: MakeRef[I, F],
      sems: MakeSemaphore[I, F]
  ) = // impl
}
```
 If you are using the same effect for creation and running 
 you can use Agents type alias defined in tofu.concurrent package object.
 ```scala
package tofu

package object concurrent {
    // other aliases...
    type Agents[F[_]] = MakeAgent[F, F]
    // other aliases...
}
```
 
 ## Examples
 Using Agents:
 
 ```scala
import cats.Monad
import cats.implicits._
import cats.syntax.flatMap._
import cats.effect.Sync
import tofu.concurrent.Agents
import tofu.common.Console
  
   def example[F[_]: Agents: Sync: Monad: Console]: F[Unit] =
        for {
          _ <- Monad[F].unit
          agent <- Agents[F].of(42)
          newValue <- agent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
          _ <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
        } yield ()
 ```
 Using MakeAgent:
 
 ```scala
import cats.Monad
import cats.implicits._
import cats.syntax.flatMap._
import cats.effect.Sync
import tofu.Fire
import tofu.concurrent.{Agents, MakeAgent, MakeRef, MakeSemaphore, Refs, Semaphores}
import tofu.common.Console
 
  def example[F[_]: Agents: Fire: Monad: Console: Sync: Refs: Semaphores](
      implicit
      refs: MakeRef[Option, F],
      sems: MakeSemaphore[Option, F]
  ): F[Unit] =
    for {
      _        <- Monad[F].unit
      agent    <- MakeAgent[Option, F].of(42).map(Monad[F].pure(_)).getOrElse(Agents[F].of(42))
      newValue <- agent.updateM(a => Console[F].putStrLn(s"current value is $a") *> Monad[F].pure(a + 27))
      _        <- Console[F].putStrLn(s"new value is $newValue") // new value is 69
   } yield ()
``` 
