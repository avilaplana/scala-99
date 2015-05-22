package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Symmetric binary trees.

//Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right
// subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a
// given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror
// image of another. We are only interested in the structure, not in the contents of the nodes.

//scala> Node('a', Node('b'), Node('c')).isSymmetric
//res0: Boolean = true

object TreeUtils {
  implicit class TreeMirror[T](left: Tree[T]) {
    def isMirrorOf(right: Tree[T]): Boolean = {
      (left, right) match {
        case (End, End) => true
        case (Node(_, a, b), Node(_, c, d)) => a.isMirrorOf(c) && b.isMirrorOf(d)
        case (_, _) => false
      }
    }
  }
  
  implicit class NodeSimmetry[T](node: Tree[T]){
    def isSymmetric = node match {
      case End => true
      case Node(v, l, r) => l.isMirrorOf(r)
    }
  }
}


class P56Spec extends WordSpecLike with Matchers {

  import TreeUtils._

  "isMirrorOf" should {
    "be true when both trees has the same structure 1" in {
      val tree1: Tree[Char] = End
      val tree2: Tree[Char] = End

      tree1.isMirrorOf(tree2) shouldBe true
    }

    "be true when both trees has the same structure 2" in {
      val tree1: Tree[Char] = Node('a', End, End)
      val tree2: Tree[Char] = Node('a', End, End)

      tree1.isMirrorOf(tree2) shouldBe true
    }

    "be true when both trees has the same structure 3" in {
      val tree1 = Node('a', Node('b'), End)
      val tree2 = Node('a', Node('b'), End)

      tree1.isMirrorOf(tree2) shouldBe true
    }

    "be true when both trees has the same structure 4" in {
      val tree1 = Node('a', End, Node('b'))
      val tree2 = Node('a', End, Node('b'))

      tree1.isMirrorOf(tree2) shouldBe true
    }

    "be true when both trees has the same structure 5" in {
      val tree1 = Node('a', Node('a', End, Node('b')), Node('b'))
      val tree2 = Node('b', Node('c', End, Node('d')), Node('f'))

      tree1.isMirrorOf(tree2) shouldBe true
    }


    "be false when both trees has no the same structure" in {
      val tree1 = Node('a', Node('a', End, Node('b')), Node('b'))
      val tree2 = Node('a', Node('a', End, Node('b')), End)

      tree1.isMirrorOf(tree2) shouldBe false
    }
  }

  "isSymmetric" should {
    "be true when the left node has the same structure as the node on the right" in {
      Node('a', End, End).isSymmetric shouldBe true
    }

    "be true when the left node has the same structure as the node on the right 2" in {
      Node('a', Node('b'), Node('c')).isSymmetric shouldBe true
    }

    "be true when the left node has the same structure as the node on the right 3" in {
      Node('a', Node('a', Node('b'), Node('c')), Node('a', Node('b'), Node('c'))).isSymmetric shouldBe true
    }

    "be true when the left node has the no same structure as the node on the right" in {
      Node('a', Node('a', Node('b'), Node('c')), Node('a', Node('b'), End)).isSymmetric shouldBe false
    }
  }

}
