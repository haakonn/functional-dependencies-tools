/*
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

// Just a shorthand for a set of functional dependencies. Each dependency A->B is
// represented as a tuple (A, B).
type Deps[A] = Set[(Set[A], Set[A])]

/**
 * Computes the transitive closure of a set of attributes under a
 * set of functional dependencies.
 */
def transitiveClosure[A](attrs: Set[A], deps: Deps[A]): Set[A] = {
  val r = deps.foldLeft (attrs) ((closure, x) => x match {
    case (a, b) if (a subsetOf closure) => closure union b
    case _ => closure
  })
  if (r == attrs) r else transitiveClosure(r, deps)
}

/**
 * Test if a set of functional dependencies is implied by (i.e., can
 * be derived from) a different set of functional dependencies.
 */
def isImplied[A](dep: (Set[A], Set[A]), deps: Deps[A]) = dep match {
  case (x, y) => y subsetOf transitiveClosure(x, deps)
}

/**
 * Test if a set of attributes determines an attribute under
 * a set of functional dependencies.
 */
def determines[A](attrs: Set[A], check: A, deps: Deps[A]) =
  transitiveClosure(attrs, deps) contains check

/**
 * Test if two sets of functional dependencies are equivalent.
 */
def equivalent[A](test: Deps[A], testAgainst: Deps[A]) =
  test.forall(isImplied(_, testAgainst))

def pair2Set(pair: (String, String)) = pair match { case (a, b) => (a toSet, b toSet) }
def tuple[A](a: Array[A]) = (a(0), a(1))

/**
 * Converts strings on the form "ABC" to a set of attributes to be used with
 * the other methods.
 */
def attrs(input: String) = input toSet

/**
 * Converts strings on the form "A->BC,D->EF" to sets of functional dependencies accepted
 * by the other methods.
 */
def deps(input: String) = input split(",") map(x => pair2Set(tuple(x split("->")))) toSet
