/-
Lean Tree Project
Cooper Braun
CPSC-326-01
Dr. Johnson
-/

-- 1. Ternary tree structure for course prerequisites

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

-- Build the full catalog tree
-- Root = CPSC 121 (no prereqs)
-- Children = courses requiring parent
/-
CPSC 121
  CPSC 122
    CPSC 223
      CPSC 326
      CPSC 351
      CPSC 450
    CPSC 224
      CPSC 391
        CPSC 491
          CPSC 492
        CPSC 499
    CPSC 260
      CPSC 346
      CPSC 348
-/
def buildCatalog : CourseTree :=
  let tree := singleNode "CPSC 121"
  let tree := insertUnder tree "CPSC 121" "CPSC 122"
  let tree := insertUnder tree "CPSC 122" "CPSC 223"
  let tree := insertUnder tree "CPSC 122" "CPSC 224"
  let tree := insertUnder tree "CPSC 122" "CPSC 260"
  let tree := insertUnder tree "CPSC 223" "CPSC 326"
  let tree := insertUnder tree "CPSC 223" "CPSC 351"
  let tree := insertUnder tree "CPSC 223" "CPSC 450"
  let tree := insertUnder tree "CPSC 224" "CPSC 391"
  let tree := insertUnder tree "CPSC 391" "CPSC 491"
  let tree := insertUnder tree "CPSC 391" "CPSC 499"
  let tree := insertUnder tree "CPSC 491" "CPSC 492"
  let tree := insertUnder tree "CPSC 260" "CPSC 346"
  let tree := insertUnder tree "CPSC 260" "CPSC 348"
  tree

-- 2. Search for a course's chained prerequisites

-- Find the prereq chain for a course
-- Takes a tree, a target course name, and an accumulator list (path so far), and returns an
-- Option (List String) - either some list of courses if found, or none if not
def findPath : CourseTree -> String -> List String -> Option (List String)
  | CourseTree.leaf, _, _ => none
  | CourseTree.node name left mid right, target, acc =>
    if name == target then
      some acc
    else
      let acc' := acc ++ [name]
      match findPath left target acc' with
      | some path => some path
      | none =>
        match findPath mid target acc' with
        | some path => some path
        | none => findPath right target acc'

-- Uses findPath in order to create the prereq chain
def prereqChain (tree : CourseTree) (course: String) : Option (List String) :=
  findPath tree course []

-- 3. Compare two trees and output completion percentage

-- Flattens a tree into a list
-- At a particular node, take this node's name as a single-element list, then concatenate it
-- with the results of recursing into all 3 children nodes. Eventually collects every course
-- in the entire tree.
def collectCourses : CourseTree -> List String
  | CourseTree.leaf => []
  | CourseTree.node name left mid right =>
    [name] ++ collectCourses left ++ collectCourses mid ++ collectCourses right

-- Check if a string is in a list
-- Destructure the list into its head (1st element) and tail (the rest). If the head matches
-- the string, return true, otherwise, recurse on the tail
def contains : List String -> String -> Bool
  | [], _ => false
  | head :: tail, string => if head == string then true else contains tail string

-- Count how many items from one list appear in another
-- Will take in a list of required courses and a list of completed courses to track progress
-- Take the first required course (head), if it appears in the done list, count it as 1 and
-- recurse on the remaining courses. If not, skip it (so add 0) and recurse
def countMatches : List String -> List String -> Nat
  | [], _ => 0
  | head :: tail, done =>
    if contains done head then 1 + countMatches tail done
    else countMatches tail done

-- Takes two trees, the full required course catalog and the courses the student has completed
def completionPercentage (required completed : CourseTree) : Nat :=
  -- Flatten required tree into a list (["CPSC 121", "CPSC 122", "CPSC 223", ...])
  let reqList := collectCourses required
  -- Flatten completed tree into a list (["CPSC 121", "CPSC 122", ...])
  let doneList := collectCourses completed
  -- Count how many courses are required total
  let total := reqList.length
  -- Count how many required courses appear in the completed list
  let matched := countMatches reqList doneList
  -- Compute the percentage of courses completed within the required curriculum
  (matched * 100) / total
