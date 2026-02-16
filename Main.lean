/-
Lean Tree Project
Cooper Braun
CPSC-326-01
Dr. Johnson
-/

-- Functions and data types (defs) use camelCase
-- Types and structures use PascalCase

-- A ternary tree where each node holds a course and up to 3 children
-- Node takes 4 arguments: String (course name) and 3 empty CourseTree values (l, m, r children)
inductive CourseTree where
  | leaf : CourseTree
  | node : String -> CourseTree -> CourseTree -> CourseTree -> CourseTree
deriving Repr, BEq

-- Quick shorthand for a node with no children
-- Creates a node with 3 leaf children
def singleNode (name : String) : CourseTree :=
  CourseTree.node name CourseTree.leaf CourseTree.leaf CourseTree.leaf

-- Insert a course as a child of a parent node
-- Fills left node, then middle node, then right node last
-- Takes in a tree, a parent name (node to insert under), and a child name (the course to insert),
-- and returns a new tree
def insertUnder : CourseTree -> String -> String -> CourseTree
  | CourseTree.leaf, _, _ => CourseTree.leaf
  | CourseTree.node name left mid right, parent, child =>
    -- If at the right node (parent node we're looking for), put the new course there
    if name == parent then
      match left, mid, right with
      -- Either we add the new course node into one of the 3 children nodes or we recurse deeper
      -- into the tree to find the correct parent node
      | CourseTree.leaf, _, _ => CourseTree.node name (singleNode child) mid right
      | _, CourseTree.leaf, _ => CourseTree.node name left (singleNode child) right
      | _, _, CourseTree.leaf => CourseTree.node name left mid (singleNode child)
      | _, _, _ => CourseTree.node name (insertUnder left parent child) (insertUnder mid parent child) (insertUnder right parent child)
    -- The current node isn't the parent we are looking for, so keep as-is and recurse into
    -- all 3 children to keep searching
    else
      CourseTree.node name (insertUnder left parent child) (insertUnder mid parent child) (insertUnder right parent child)
