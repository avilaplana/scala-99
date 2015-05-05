package alvaro

import org.scalatest.{Matchers, WordSpecLike}

//Symmetric binary trees.

//Let us call a binary tree symmetric if you can draw a vertical line through the root node and then the right
// subtree is the mirror image of the left subtree. Add an isSymmetric method to the Tree class to check whether a
// given binary tree is symmetric. Hint: Write an isMirrorOf method first to check whether one tree is the mirror
// image of another. We are only interested in the structure, not in the contents of the nodes.

//scala> Node('a', Node('b'), Node('c')).isSymmetric
//res0: Boolean = true

object P56 {
  implicit class TreeUtils(left: Tree[Char]) {
    def isSymmetric(right: Tree[Char]): Boolean = {
      (left, right) match {
        case (Node(_, End, End), Node(_, End, End)) => true
        case (Node(_, a, End), Node(_, c, End)) => a.isSymmetric(c)
        case (Node(_, End, b), Node(_, End, d)) => b.isSymmetric(d)
        case (Node(_, a, b), Node(_, c, d)) => a.isSymmetric(c) && b.isSymmetric(d)
        case (_, _) => false
      }
    }
  }
}


class P56Spec extends WordSpecLike with Matchers {

  import P56._

  "isMirrorOf" should {
    "be true when the tree in the left is the same than the tree in the right 1" in {
      val tree1: Tree[Char] = Node('a', End, End)
      val tree2: Tree[Char] = Node('a', End, End)

      tree1.isSymmetric(tree2) shouldBe true
    }

    "be true when the tree in the left is the same than the tree in the right 2" in {
      val tree1 = Node('a', Node('b'), End)
      val tree2 = Node('a', Node('b'), End)

      tree1.isSymmetric(tree2) shouldBe true
    }

    "be true when the tree in the left is the same than the tree in the right 3" in {
      val tree1 = Node('a', End, Node('b'))
      val tree2 = Node('a', End, Node('b'))

      tree1.isSymmetric(tree2) shouldBe true
    }

    "be true when the tree in the left is the same than the tree in the right 4" in {
      val tree1 = Node('a', Node('a', End, Node('b')), Node('b'))
      val tree2 = Node('b', Node('c', End, Node('d')), Node('f'))

      tree1.isSymmetric(tree2) shouldBe true
    }


    "be false when the tree in the left is the same than the tree in the right 5" in {
      val tree1 = Node('a', Node('a', End, Node('b')), Node('b'))
      val tree2 = Node('a', Node('a', End, Node('b')), End)

      tree1.isSymmetric(tree2) shouldBe false
    }
  }

}
