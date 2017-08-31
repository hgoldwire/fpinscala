import fpinscala.datastructures._

val t = Branch(
  Branch(
    Leaf(1),
    Leaf(2)
  ),
  Branch(
    Leaf(3),
    Branch(
      Leaf(4),
      Leaf(5)
    )
  ),
)

Tree.