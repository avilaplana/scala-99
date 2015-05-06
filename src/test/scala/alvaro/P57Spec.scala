package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Write a function to add an element to a binary search tree.
//scala> End.addValue(2)
//res0: Node[Int] = T(2 . .)
//
//scala> res0.addValue(3)
//res1: Node[Int] = T(2 . T(3 . .))
//
//scala> res1.addValue(0)
//res2: Node[Int] = T(2 T(0 . .) T(3 . .))
//Hint: The abstract definition of addValue in Tree should be def addValue[U >: T <% Ordered[U]](x: U): Tree[U].
// The >: T is because addValue's parameters need to be contravariant in T. (Conceptually, we're adding nodes above
// existing nodes. In order for the subnodes to be of type T or any subtype, the upper nodes must be of type T or
// any supertype.) The <% Ordered[U] allows us to use the < operator on the values in the tree.
//
//Use that function to construct a binary tree from a list of integers.
//
//scala> Tree.fromList(List(3, 2, 5, 7, 1))
//res3: Node[Int] = T(3 T(2 T(1 . .) .) T(5 . T(7 . .)))
//Finally, use that function to test your solution to P56.

//scala> Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric
//res4: Boolean = true

//scala> Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric
//res5: Boolean = false

object P57 {

  implicit class TreeAdder[T](t: Tree[T]) {
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U] =
      t match {
        case End => Node(x)
        case Node(a, b, c) if x <= a => Node(a, b.addValue(x), c)
        case Node(a, b, c) if x > a => Node(a, b, c.addValue(x))
      }
  }

  def fromList[T, U >: T <% Ordered[U]](l: List[U]): Tree[U] = l.foldLeft(End.asInstanceOf[Tree[U]]) { (t, e) => t.addValue(e)}

}

class P57Spec extends WordSpecLike with Matchers {

  import P57._

  "addValue" should {
    "create a T(3 . .) when insert 3 in empty root" in {
      End.addValue(3) shouldBe Node(3)
    }

    "create T(3 T(2 ..)) when add node 2 to T(3 . .)" in {
      Node(3).addValue(2) shouldBe Node(3, Node(2), End)
    }

    "create T(3 T(2 . .) T(5 . .)) when add node 5 to T(3 T(2 ..))" in {
      Node(3).addValue(2).addValue(5) shouldBe Node(3, Node(2), Node(5))
    }

    "create T(3 T(2 . .) T(5 . T(7 ..))) when add node 7 to T(3 T(2 . .) T(5 . .))" in {
      Node(3).addValue(2).addValue(5).addValue(7) shouldBe Node(3, Node(2), Node(5, End, Node(7)))
    }

    "create T(3 T(2 T(1 . .) .) T(5 . T(7 ..))) when add node 1 to T(3 T(2 . .) T(5 . T(7 ..)))" in {
      Node(3).addValue(2).addValue(5).addValue(7).addValue(1) shouldBe Node(3, Node(2, Node(1), End), Node(5, End, Node(7)))
    }

    "create T(3 T(2 T(1 . .) .) T(5 . T(7 . .))) when add node 7 to T(3 T(2 T(1 . .) .) T(5 . .))" in {
      Node(2).addValue(3) shouldBe Node(2, End, Node(3))
    }
  }

  "fromList" should {
    import TreeUtils._

    "create a Tree from a list List(5, 3, 18, 1, 4, 12, 21)" in {
      val tree: Tree[Int] = fromList(List(5, 3, 18, 1, 4, 12, 21))
      tree shouldBe Node(5, Node(3, Node(1), Node(4)), Node(18, Node(12), Node(21)))
      tree.isSymmetric shouldBe true
    }
  }

}
