{-
TREE IMPLEMENTATION IN HASKELL
    Author:
        Created: March 20, 2018
            by David Osorio
        Modified: April 12, 2018 21:22:38
            by David Osorio
    
    Organization:
        Colima Institute of Technology
    
    Career:
        Computer Engineering
    
    Subject:
        Logic and Functional Programming
-}



-------------------------------------------
--Created: March 20, 2018 09:21:34
--Modified: April 11, 2018 10:58:30

{-
 * REFERENCES:
 * https://www.geeksforgeeks.org/binary-tree-set-1-introduction (C, Java and Python codes)
 * https://www.geeksforgeeks.org/binary-tree-set-2-properties
 * https://www.geeksforgeeks.org/binary-tree-set-3-types-of-binary-tree
 * https://www.geeksforgeeks.org/convert-normal-bst-balanced-bst (C and Java codes)
 * https://www.geeksforgeeks.org/expression-tree (C, Java and Python codes)
 *
 * http://typeocaml.com/2014/11/26/height-depth-and-level-of-a-tree (Properties of trees explained)
 * https://en.wikipedia.org/wiki/Tree_%28data_structure%29#Terminology_used_in_trees (All terms)
 * https://stackoverflow.com/questions/2603692/what-is-the-difference-between-tree-depth-and-height
 *
 * http://learnyouahaskell.com/zippers (Data structure without Data.Tree library)
 * https://dkalemis.wordpress.com/2014/01/23/trees-in-haskell (Nice and clear tutorial)
 * http://www.glc.us.es/~jalonso/vestigium/i1m2014-ejercicios-sobre-arboles-binarios-en-haskell (Nice examples)
 *
 * https://gist.github.com/Kedrigern/1239141 (Implementation of BST script)
 * https://arjunkrishnababu96.github.io/tree-implementation-in-haskell ( data Tr a = Leaf a | Node (Tr a) (Tr a) )
 *
 * https://algs4.cs.princeton.edu/32bst (Binary Search Trees General Info and Operations)
 * https://hackage.haskell.org/package/pretty-tree-0.1.0.0/docs/Data-Tree-Pretty.html#v:drawVerticalTree (???)
 * https://stackoverflow.com/questions/25714756/building-a-tree-in-haskell (Insertion)
 *
 * https://www.quora.com/How-many-null-nodes-will-a-binary-tree-with-20-nodes-have (Null nodes = n + 1)
 * Consider a binary tree of n nodes.
 * Each node will have 2 pointers (may or may not be null). So the tree will have 2n pointers.
 * There are n nodes. Excluding the root node, every node must have a pointer pointing to it, i.e.,
 * n-1 not-null pointers. So, number of null pointers (null nodes) = 2n - (n - 1) = n + 1
 *
 * https://wiki.haskell.org/Type (Data declaration and 52-card deck example)
 * One introduces, or declares, a type in Haskell via the 'data' statement.
 * The essence of the statement is that you use the keyword data, supply an optional context, give the type name
 * and a variable number of type variables. This is then followed by a variable number of constructors, each of
 * which has a list of type variables or type constants. At the end, there is an optional deriving.
-}


--Imports only the Tree data type and its Node constructor from Data.Tree
--Qualified imports mean that all the imported values include the module names as a prefix
--import qualified Data.Tree (Tree(Node))
--import Data.Tree.Pretty (drawVerticalTree)


{-
 * This statement defines a binary tree that takes a value of type a. Because we have not explicitly specified
 * any datatype (such as an Int or a [Char], for instance) and instead defined it in terms of a which can stand
 * for any type, this tree is capable of handling values of any datatype.
 * L (Leaf) and N (Node) are CONSTRUCTORS, Tr (Tree) is a DEFINED TYPE, i.e. the tree data structure.
 * 'Leaf' is the same as 'Empty', 'Nil' or 'Null' node (Teacher and LearnYouAHaskell's tree).
 * Show typeclass is required to print the binary tree as a string;
 * Eq is used for types that support equality testing (== and /=).
 * Directions: Select one tree data structure (Teacher's or Internet's) and comment next three tree definitions.
-}
--Teacher and LearnYouAHaskell's data tree structure
{- data Tr a = L
    | N a (Tr a) (Tr a)
    deriving (Show, Eq) -}


--Internet's data tree structure
--Source: http://www.glc.us.es/~jalonso/vestigium/i1m2014-ejercicios-sobre-arboles-binarios-en-haskell
data Tr a = Null
    | L a
    | N a (Tr a) (Tr a)
    deriving (Show, Eq)


tree1 :: Tr Integer
--Teacher and LearnYouAHaskell's tree
--tree1 = N 10 (N 3 L L) (N 4 L L)
--Internet's tree
tree1 = N 10 (L 3) (L 4)
{-
    10
    /\
   3  4
Properties: 2 leaves, 3 nodes, 1 branch, depth of tree: 1, isBST: False
-}


tree2 :: Tr Integer
--Teacher and LearnYouAHaskell's tree
{- tree2 = N 36
        (N 25
            (N 9 L L)
            (N 10 L L)
        )
        (N 2
            (N 4 L L)
            (N 7
                (N 6 L L)
                L
            )
        ) -}
--Internet's tree
tree2 = N 36 (N 25 (L 9) (L 10)) (N 2 (L 4) (N 7 (L 6) Null))
{-
        36
        / \
      25   2
      /\   /\
     9 10 4  7
             /
            6
Properties: 4 leaves, 8 nodes, 4 branches, depth of tree: 3, isBST: False
-}


tree3 :: Tr Char
--Teacher and LearnYouAHaskell's tree
{- tree3 =
    N 'P'
        (N 'O'
            (N 'L'
                (N 'N' L L)
                (N 'T' L L)
            )
            (N 'Y'
                (N 'S' L L)
                (N 'A' L L)
            )
        )
        (N 'L'
            (N 'W'
                (N 'C' L L)
                (N 'R' L L)
            )
            (N 'A'
                (N 'A' L L)
                (N 'C' L L)
            )
        ) -}
--Internet's tree
tree3 = N 'P'
        ( (N 'O') (N 'L' (L 'N') (L 'T')) (N 'Y' (L 'S') (L 'A')) )
        ( (N 'L') (N 'W' (L 'C') (L 'R')) (N 'A' (L 'A') (L 'C')) )
{-
 * Visual representation at: http://learnyouahaskell.com/zippers#taking-a-walk
 * Properties: 8 leaves, 15 nodes, 7 branches, depth of tree: 3, isBST: Error, expected type: Tr Integer
-}


--Internet's tree
tree4 :: Tr Integer
tree4 = N 36 (N 25 (N 6 Null (N 18 Null (L 20))) Null) (N 93 (L 40) (L 120))
{-
        36
        / \
      25   93
      /    /\
     6   40 120
      \
      18
        \
        20
Properties: 3 leaves, 8 nodes, 5 branches, depth of tree: 4, isBST: True
-}
--Convert a list to a BST example
tree4list :: [Integer]
tree4list = [36,25,6,18,20,93,40,120]       --tree4 == listToBST tree4list -> True
--tree4list = [36,25,6,18,20,93,120,40]     --tree4 == listToBST tree4list -> True
--tree4list = [36,25,6,20,18,93,120,40]     --tree4 == listToBST tree4list -> False


--Internet's tree
tree5 :: Tr Integer
tree5 = N 40 (N 25 (L 12) (L 27)) (N 88 Null (L 99))
{-
        40
        / \
       25  88
       /\    \
      12 27   99
Properties: 3 leaves, 6 nodes, 3 branches, depth of tree: 2, isBST: True
-}
--Convert a list to a BST example
tree5list :: [Integer]
tree5list = [40,25,27,12,88,99]             --tree5 == listToBST tree5list -> True
--tree5list = [40,25,12,27,88,99]           --tree5 == listToBST tree5list -> True
--tree5list = [40,12,25,17,88,99]           --tree5 == listToBST tree5list -> False


--Internet's tree
tree6 :: Tr Integer
tree6 = N 32 (N 6 (L 3) (N 20 (L 13) Null)) (Null)
{-
    32
    /
   6
  / \
 3  20
    /
   13
Properties: 2 leaves, 5 nodes, 4 branches, depth of tree: 3, isBST: True
-}
--Convert a list to a BST example
tree6list :: [Integer]
tree6list = [32,6,20,13,3]                  --tree6 == listToBST tree6list -> True
--tree6list = [32,6,3,20,13]                --tree6 == listToBST tree6list -> True
--tree6list = [32,6,13,20,3]                --tree6 == listToBST tree6list -> False


--Returns the number of leaves in a tree. From now on, lT stands for left subtree and rT for right subtree
nLeaves :: Tr a -> Int
nLeaves Null            = 0
nLeaves (L _)           = 1
nLeaves (N a lT rT)     = nLeaves lT + nLeaves rT


--Returns the number of nodes in a tree
nNodes :: Tr a -> Int
nNodes Null             = 0
nNodes (L _)            = 1
nNodes (N a lT rT)      = 1 + nNodes lT + nNodes rT


--Returns the number of branches in a tree, i.e. nodes with at least one child
nBranches :: Tr a -> Int
nBranches Null          = 0
nBranches (L _)         = 0
nBranches (N a lT rT)   = nNodes (N a lT rT) - nLeaves (N a lT rT)


--Returns the depth of a tree. Height and depth of a tree is equal
--The height of a tree would be the height of its root node, or equivalently, the depth of its deepest node
--Find more at: https://stackoverflow.com/questions/2603692/what-is-the-difference-between-tree-depth-and-height
treeDepth :: Tr a -> Int
treeDepth Null          = 0
treeDepth (L _)         = 1
treeDepth (N a lT rT)   = 1 + max (treeDepth lT) (treeDepth rT)


--Prints a binary expression tree using prefix notation (Polish notation), aka pre-order traversal or tree search
--Find more at: https://en.wikipedia.org/wiki/Tree_traversal
expPrefix :: Tr a -> [a]
expPrefix Null          = []
expPrefix (L a)         = [a]
expPrefix (N a lT rT)   = [a] ++ expPrefix lT ++ expPrefix rT


--Prints a binary expression tree using infix notation, aka in-order traversal or tree search
expInfix :: Tr a -> [a]
expInfix Null           = []
expInfix (L a)          = [a]
expInfix (N a lT rT)    = expInfix lT ++ [a] ++ expInfix rT


--Prints a binary expression tree using postfix notation (RPN), aka pos-order traversal or tree search
expPostfix :: Tr a -> [a]
expPostfix Null         = []
expPostfix (L a)        = [a]
expPostfix (N a lT rT)  = expPostfix lT ++ expPostfix rT ++ [a]



-------------------------------------------
--Created: April 09, 2018 09:55:44 (After Holy Week break)
--Modified: April 12, 2018 21:22:32

--Returns true if a integer list is sorted in ascending order
--Source: https://stackoverflow.com/questions/10560782/validating-binary-search-tree-beginner-at-haskell
isAscending :: [Integer] -> Bool
isAscending [x]     = True
isAscending (x:xs)  = x <= head (xs) && isAscending xs


{-
 * Tells whether a given tree is a Binary Search Tree (BST):
 * Traversal.
 * Once the binary search tree has been created, its elements can be RETRIEVED IN-ORDER by recursively traversing
 * the left subtree of the root node, accessing the node itself, then recursively traversing the right subtree of
 * the node, continuing this pattern with each node in the tree as it's recursively accessed. As with all binary
 * trees, one may conduct a pre-order traversal or a post-order traversal, but neither are likely to be useful
 * for binary search trees. An in-order traversal of a binary search tree will always RESULT IN A SORTED LIST of
 * node items (numbers, strings or other comparable items).
 * Verification.
 * The BST property-every node on the right subtree has to be larger than the current node and every node on the
 * left subtree has to be smaller than the current node-is the key to figuring out whether a tree is a BST or not.
 * The greedy algorithm-simply traverse the tree, at every node check whether the node contains a value larger
 * than the value at the left child and smaller than the value on the right child-does not work for all cases.
 * Find more at: https://en.wikipedia.org/wiki/Binary_search_tree
-}
isBST :: Tr Integer -> Bool
isBST = isAscending . expInfix


--Returns true if a BST contains an integer value
--In case the tree is neither null nor only a leaf, checks whether it's a BST
elemBST :: Integer -> Tr Integer -> Bool
elemBST n Null                      = False
elemBST n (L a)
    | n == a                        = True
    | otherwise                     = False
elemBST n (N a lT rT)
    | isBST (N a lT rT) == True     = elemBSTaux n (N a lT rT)
    | otherwise                     = error "The function cannot search the element because the tree is not a BST"


--Searchs an element in a BST. This works if and only if the given tree is a BST
--Note: This is an auxiliary function called by elemBST. Unexpected output may happen if it is run by user
elemBSTaux :: Ord a => a -> Tr a -> Bool
elemBSTaux n Null   = False
elemBSTaux n (L a)
    | n == a        = True
    | otherwise     = False
elemBSTaux n (N a lT rT)
    | n == a        = True
    | n > a         = elemBSTaux n rT
    | otherwise     = elemBSTaux n lT


--Inserts a new value (it's always a leaf) into an ordered BST
insertBST :: Ord a => a -> Tr a -> Tr a
--Pattern matching (PM 1)
insertBST n Null    = (L n)
--Pattern matching (PM 2)
insertBST n (L a)
    | n > a         = (N a Null (L n))
    | otherwise     = (N a (L n) Null)
--Pattern matching (PM 3)
insertBST n (N a lT rT)
    | n > a         = (N a lT (insertBST n rT))
    | otherwise     = (N a (insertBST n lT) rT)


{- Insertion Examples
        36
        / \
      25   93
      /    /\
     6   40 120
      \
      18
        \
        20
                tree4 = N 36 (N 25 (N 6 Null (N 18 Null (L 20))) Null) (N 93 (L 40) (L 120))
>insertBST 30   tree4 = N 36 (N 25 (N 6 Null (N 18 Null (L 20))) 30) (N 93 (L 40) (L 120)) [PM 3 & PM 1]
>insertBST 55   tree4 = N 36 (N 25 (N 6 Null (N 18 Null (L 20))) Null) (N 93 (N 40 Null (L 55)) (L 120)) [PM 2]
>The list must be in ascending order after running: expInfix (insertBST [n] [tree])
-}


--Converts an unordered list into a BST using recursive calls and pattern matching
listToBST :: Ord a => [a] -> Tr a
listToBST []        = Null
listToBST [x]       = (L x)
listToBST xs        = insertBST (last xs) (listToBST (init xs))


{- List to BST Example
tree61 = Null
tree62 = (L 32)
tree63 = N 32 (L 6) (Null)
tree64 = N 32 (N 6 (L 3) Null) (Null)
tree65 = N 32 (N 6 (L 3) (L 20)) (Null)
tree66 = N 32 (N 6 (L 3) (N 20 (L 13) Null)) (Null) --It's the same as previous declared BST tree6
-}