package tofu.optics

import cats.{Applicative, Monoid}
import monocle.*
import scala.annotation.unchecked.uncheckedVariance

object interop {
  implicit final class EquivalentInteropOps[S, T, A, B](private val eqv: PEquivalent[S, T, A, B]) extends AnyVal {
    def toIso: PIso[S, T, A, B] = PIso(eqv.extract)(eqv.upcast)
  }

  implicit final class MonocleIsoInteropOps[S, T, A, B](private val eqv: PIso[S, T, A, B]) extends AnyVal {
    def toEquivalent: PEquivalent[S, T, A, B] =
      new PEquivalent[S, T, A, B] {
        def extract(s: S): A = eqv.get(s)
        def back(b: B): T    = eqv.reverseGet(b)
      }
  }

  implicit final class ContainsInteropOps[S, T, A, B](private val conts: PContains[S, T, A, B]) extends AnyVal {
    def toLens: PLens[S, T, A, B] = PLens(conts.extract)(b => s => conts.set(s, b))
  }

  implicit final class MonocleLensInteropOps[S, T, A, B](private val lens: PLens[S, T, A, B]) extends AnyVal {
    def toContains: PContains[S, T, A, B] = new PContains[S, T, A, B] {
      def set(s: S, b: B): T = lens.set(b)(s)
      def extract(s: S): A   = lens.get(s)
    }
  }

  implicit final class SubsetInteropOps[S, T, A, B](private val conts: PSubset[S, T, A, B]) extends AnyVal {
    def toPrism: PPrism[S, T, A, B] = PPrism(conts.narrow)(conts.upcast)
  }

  implicit final class MonoclePrismInteropOps[S, T, A, B](private val lens: PPrism[S, T, A, B]) extends AnyVal {
    def toSubset: PSubset[S, T, A, B] = new PSubset[S, T, A, B] {
      def narrow(s: S): Either[T, A] = lens.getOrModify(s)
      def upcast(b: B): T            = lens.reverseGet(b)
    }
  }

  implicit final class PropertyInteropOps[S, T, A, B](private val conts: PProperty[S, T, A, B]) extends AnyVal {
    def toOptional: POptional[S, T, A, B] = POptional(conts.narrow)(b => s => conts.set(s, b))
  }

  implicit final class MonocleOptionalInteropOps[S, T, A, B](private val opt: POptional[S, T, A, B]) extends AnyVal {
    def toProperty: PProperty[S, T, A, B] = new PProperty[S, T, A, B] {
      def set(s: S, b: B): T         = opt.set(b)(s)
      def narrow(s: S): Either[T, A] = opt.getOrModify(s)
    }
  }

  implicit final class ItemsInteropOps[S, T, A, B](private val items: PItems[S, T, A, B]) extends AnyVal {
    def toTraversal: PTraversal[S, T, A, B] = new PTraversal[S, T, A, B] {
      def modifyF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = {
        type F1[+x] = F[x @uncheckedVariance]
        items.traverse[F1](s)(f)
      }
    }
  }

  implicit final class TraversalInteropOps[S, T, A, B](private val items: PTraversal[S, T, A, B]) extends AnyVal {
    def toItems: PItems[S, T, A, B] = new PItems[S, T, A, B] {
      def traverse[F[_]: Applicative](a: S)(f: A => F[B]): F[T] = items.modifyF(f)(a)
    }
  }

  implicit final class FoldedInteropOps[S, T, A, B](private val folded: PFolded[S, T, A, B]) extends AnyVal {
    def toFold: Fold[S, A] = new Fold[S, A] {
      def foldMap[M: Monoid](f: A => M)(s: S): M = folded.foldMap(s)(f)
    }
  }

  implicit final class FoldInteropOps[S, A](private val fold: Fold[S, A]) extends AnyVal {
    def toFolded[T, B]: PFolded[S, T, A, B] = new PFolded[S, T, A, B] {
      def foldMap[X: Monoid](s: S)(f: A => X): X = fold.foldMap(f)(s)
    }

    def toFoldedMono: Folded[S, A] = toFolded[S, A]
  }

  implicit final class UpdateInteropOps[S, T, A, B](private val update: PUpdate[S, T, A, B]) extends AnyVal {
    def toSetter: PSetter[S, T, A, B] = new PSetter[S, T, A, B] {
      def modify(f: A => B): S => T = update.update(_, f)
      def set(b: B): S => T         = update.put(_, b)
    }
  }

  implicit final class SettterInteropOps[S, T, A, B](private val setter: PSetter[S, T, A, B]) extends AnyVal {
    def toUpdate: PUpdate[S, T, A, B] = (s, fb) => setter.modify(fb)(s)
  }

  implicit final class ExtractInteropOps[S, T, A, B](private val extract: PExtract[S, T, A, B]) extends AnyVal {
    def toGetter: Getter[S, A] = extract.extract
  }

  implicit final class GetterInteropOps[S, A](private val getter: Getter[S, A]) extends AnyVal {
    def toExtract[T, B]: PExtract[S, T, A, B] = getter.get
    def toExtractMono: Extract[S, A]          = toExtract[S, A]
  }

}
