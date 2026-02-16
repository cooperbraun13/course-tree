/-
Lean Tree Project
Cooper Braun
CPSC-326-01
Dr. Johnson
-/

-- Functions and data types (defs) use camelCase
-- Types and structures use PascalCase

-- A ternary tree where each node holds a course and up to 3 children
inductive CourseTree where
  | leaf : CourseTree
  | node : String -> CourseTree -> CourseTree -> CourseTree -> CourseTree
deriving Repr, BEq

-- Quick shorthand for a node with no children
def singleNode (name : String) : CourseTree :=
  CourseTree.node name CourseTree.leaf CourseTree.leaf CourseTree.leaf
