package chapter3

import org.scalatest.FreeSpec

/**
  * Created by lbajor on 2016-05-27.
  */
class TreeShould extends FreeSpec {

  "size" - {
    "return 1 for leaf" in {
      assert(1 === Tree.size(Leaf(1)))
    }

    "return nodes count" in {
      assert(5 === Tree.size(Branch(Leaf(1), Branch(Leaf(2),Leaf(3)))))
    }
  }

  "maximum" - {
    "return 1 for leaf" in {
      assert(1 === Tree.maximum(Leaf(1)))
    }

    "return max value in leafs" in {
      assert(3 === Tree.maximum(Branch(Leaf(1), Branch(Leaf(2),Leaf(3)))))
    }
  }

  "depth" - {
    "return 1 for leaf" in {
      assert(1 === Tree.depth(Leaf(1)))
    }

    "return deepest branch depth + 1" in {
      assert(4 === Tree.depth(Branch(Leaf(1), Branch(Leaf(2),Branch(Leaf(3), Leaf(4))))))
    }
  }

  "map" - {
    "return Leaf(\"1\") for Leaf(1)" in {
      assert(Leaf("1") === Tree.map(Leaf(1))(_.toString))
    }

    "return branch of string leaves for branch of ints" in {
      assert(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))) === Tree.map(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(_.toString))
    }
  }
}
