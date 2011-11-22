{-------------------------------------------------------------------------------
 - Max Guo
 - November 21, 2011
 - AVL Tree
 ------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
 - TODO: - avlDelete doesnÕt fully work, doesnÕt appear to rebalance correctly
 -             if at all, given example for delete works
 -       - avlInsert not fully tested, may have bugs
 ------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
 - USAGE: - main only runs a few hard coded tests
 -
 -              can add more test cases, use current test cases as examples
 -              or run directly in ghci
 ------------------------------------------------------------------------------}
 
{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}

module Main where

import Test.HUnit

data AVL e = E           -- empty tree
           | N           -- non-empty tree
               Int           -- balance factor 
               Int           -- height
               (AVL e)       -- left subtree
               e             -- value
               (AVL e)       -- right subtree
  deriving (Show, Eq)

-- an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- create a new AVL tree with the given element and left and right subtrees
node :: AVL e -> e -> AVL e -> AVL e
node l x r = N bf ht l x r where
  ht = 1 + max (height l) (height r)
  bf = height l - height r

-- Access the height component of the AVL tree
height :: AVL e -> Int
height E              = 0
height (N _ ht _ _ _) = ht

-- Determine if an element is contained within the tree
avlLookup :: Ord e => e -> AVL e -> Bool
avlLookup _ E = False
avlLookup x (N _ _ a y b)
    | x < y = avlLookup x a
    | x > y = avlLookup x b
    | otherwise = True

-- The height at each node is correctly calculated. 
prop_ht :: Show e => AVL e -> Either String ()
prop_ht E
    | height E == 0 = Right ()
    | otherwise = Left "Height calculated incorrectly."
prop_ht x@(N _ _ a _ b)
    | height x == (1 + max (height a) (height b))
        && prop_ht a == Right () && prop_ht b == Right () = Right ()
    | otherwise = Left "Height calculated incorrectly."

-- The balance factor at each node is correctly calculated.  
prop_bf :: Show e => AVL e -> Either String ()
prop_bf E = Right ()
prop_bf (N bf _ a _ b)
    | bf == height a - height b
        && prop_bf a == Right () && prop_bf b == Right () = Right ()
    | otherwise = Left "Balance Factor calculated incorrectly."

-- The balance factor at each node is between -1 and +1.  
prop_balance :: Show e => AVL e -> Either String ()    
prop_balance E = Right ()
prop_balance (N bf _ a _ b)
    | abs bf <= 1
        && prop_balance a == Right () && prop_balance b == Right () = Right ()
    | otherwise = Left "Incorrect Balance Factor"

-- The items stored in the tree are in strictly increasing order.
prop_inorder :: (Ord e, Show e) => AVL e -> Either String ()
prop_inorder E = Right ()
prop_inorder (N _ _ E _ E) = Right ()
prop_inorder (N _ _ E x b@(N _ _ _ y _))
    | x < y && prop_inorder b == Right () = Right ()
    | otherwise = Left "AVL Tree not in order."
prop_inorder (N _ _ a@(N _ _ _ y _) x E)
    | y < x && prop_inorder a == Right () = Right ()
    | otherwise = Left "AVL Tree not in order."
prop_inorder (N _ _ a@(N _ _ _ y1 _) x b@(N _ _ _ y2 _))
    | y1 < x && x < y2 &&
        prop_inorder a == Right () && prop_inorder b == Right () = Right ()
    | otherwise = Left "AVL Tree not in order."

t1 :: AVL Int
t1 = E

t2 :: AVL Int
t2 = node E 1 E

t3 :: AVL Int
t3 = node (node E 1 E) 2 E

t4 :: AVL Int
t4 = node (node E 1 E) 2 (node E 3 E)

t5 :: AVL Int
t5 = node (node E 1 E) 2 (node E 3 (node E 5 E))

t6 :: AVL Int
t6 = N 0 3 (N 0 2 (N 0 1 E 1 E) 2 (N 0 1 E 3 E))
           5
           (N 0 2 (N 0 1 E 6 E) 10 (N 0 1 E 11 E))

bad1 :: AVL Int
bad1 = node (node E 1 E) 2 (node E 3 (node E 5 (node E 8 E)))

bad2 :: AVL Int
bad2 = node (node E 6 E) 2 (node E 3 (node E 5 E))

bad3 :: AVL Int
bad3 = N (-1) 3 (N 0 2 (N 0 1 E 1 E) 2 (N 0 1 E 3 E))
           5
           (N 0 2 (N 0 1 E 6 E) 10 (N 0 1 E 11 E))

bad4 :: AVL Int
bad4 = N 0 3 (N 0 4 (N 0 1 E 1 E) 2 (N 0 1 E 3 E))
           5
           (N 0 2 (N 0 1 E 6 E) 10 (N 0 1 E 11 E))

-- Check all invariants of an AVL tree
check :: (Ord e, Show e) => AVL e -> Either String ()
check x =
    if a /= Right ()
        then a
        else if b /= Right ()
            then b
            else if c /= Right ()
                then c
                else if d /= Right ()
                    then d
                    else Right ()
    where a = prop_ht x
          b = prop_bf x
          c = prop_balance x
          d = prop_inorder x

testCheck :: Test
testCheck =
    TestList[check t1 ~?= Right (),
             check t2 ~?= Right (),
             check t3 ~?= Right (),
             check t4 ~?= Right (),
             check t5 ~?= Right (),
             check t6 ~?= Right (),
             check bad1 ~?= Left "Incorrect Balance Factor",
             check bad2 ~?= Left "AVL Tree not in order.",
             check bad3 ~?= Left "Balance Factor calculated incorrectly.",
             check bad4 ~?= Left "Height calculated incorrectly."]

main :: IO ()
main = do
       _ <- runTestTT $ TestList[testCheck, testInsert, testDelete]
       return ()

rebalanceLL :: (Ord e) => AVL e -> AVL e
rebalanceLL (N _ _ (N _ _ ll xl lr) x r) = node ll xl (node lr x r)
rebalanceLL t = t

rebalanceLR :: (Ord e) => AVL e -> AVL e
rebalanceLR (N _ _ (N _ _ ll xl (N _ _ lrl xlr lrr)) x r)
    = node (node ll xl lrl) xlr (node lrr x r)
rebalanceLR t = t

rebalanceRL :: (Ord e) => AVL e -> AVL e
rebalanceRL (N _ _ l x (N _ _ (N _ _ rll xrl rlr) xr rr))
    = node (node l x rll) xrl (node rlr xr rr)
rebalanceRL t = t

rebalanceRR :: (Ord e) => AVL e -> AVL e
rebalanceRR (N _ _ l x (N _ _ rl xr rr)) = node (node l x rl) xr rr
rebalanceRR t = t

getElem :: AVL e -> e
getElem (N _ _ _ x _) = x
getElem E = error "getElem error, empty tree has no elements"

getLeft :: AVL e -> AVL e
getLeft (N _ _ l _ _) = l
getLeft E = E

getRight :: AVL e -> AVL e
getRight (N _ _ _ _ r) = r
getRight E = E

-- Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert x E = node E x E
avlInsert x t@(N _ _ l y r)
    | x == y = t
    | x < y && bf1 == 2 && x < getElem l = rebalanceLL (node li y r)
    | x < y && bf1 == 2 && x > getElem l = rebalanceLR (node li y r)
    | x > y && bf2 == -2 && x < getElem r = rebalanceRL (node l y ri)
    | x > y && bf2 == -2 && x > getElem r = rebalanceRR (node l y ri)
    | x < y = node li y r
    | x > y = node l y ri
    | otherwise = error "avlInsert error, case not possible"
        where li = avlInsert x l
              ri = avlInsert x r
              bf1 = height li - height r
              bf2 = height l - height ri

checkInsert :: (Show e, Ord e) => e -> AVL e -> Either String (AVL e)
checkInsert x t = if a /= Right ()
                      then Left s
                      else if not checkElem
                               then Left "Elements missing in new tree."
                               else Right b
                  where a = check b
                        Left s = check b
                        b = avlInsert x t
                        c = inOrderWalk t
                        d = inOrderWalk b
                        checkElem = and (map (`elem` d) c)

testInsert :: Test
testInsert =
    TestList[checkInsert 14 (node (node E 13 E) 15 E)
             ~?= Right (N 0 2 (N 0 1 E 13 E) 14 (N 0 1 E 15 E)),
             checkInsert 13 (node (node E 14 E) 15 E)
             ~?= Right (N 0 2 (N 0 1 E 13 E) 14 (N 0 1 E 15 E)),
             checkInsert 16 (node E 15 (node E 17 E))
             ~?= Right (N 0 2 (N 0 1 E 15 E) 16 (N 0 1 E 17 E)),
             checkInsert 17 (node E 15 (node E 16 E))
             ~?= Right (N 0 2 (N 0 1 E 15 E) 16 (N 0 1 E 17 E)),
             checkInsert 11 (node (node (node E 1 E) 2 (node E 3 E)) 5 (node E 10 E))
             ~?= Right (N 0 3 (N 0 2 (N 0 1 E 1 E) 2 (N 0 1 E 3 E)) 5 (N (-1) 2 E 10 (N 0 1 E 11 E))),
             checkInsert (-1) (node (node (node E 1 E) 2 (node E 3 E)) 5 (node E 10 E))
             ~?= Right (N 0 3 (N 1 2 (N 0 1 E (-1) E) 1 E) 2 (N 0 2 (N 0 1 E 3 E) 5 (N 0 1 E 10 E)))]

inOrderWalk :: AVL e -> [e]
inOrderWalk E = []
inOrderWalk (N _ _ l x r) = inOrderWalk l ++ [x] ++ inOrderWalk r

findMin :: AVL e -> e
findMin t = if not (null elems)
                then head elems
                else error "findMin error, empty tree"
            where elems = inOrderWalk t

avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete _ E = E
avlDelete x t@(N _ _ E y E) = if x == y then E else t
avlDelete x t@(N _ _ l y E) = if x == y then l else t
avlDelete x t@(N _ _ E y r) = if x == y then r else t
avlDelete x (N _ _ l y r)
    | x == y = node l rmin rtrimmin
    | x < y && abs (height ltrim - height r) < 2 = node ltrim y r
    | x > y && abs (height l - height rtrim) < 2 = node l y rtrim
    | x < y && height (getLeft r) - height (getRight r) < 0
        = rebalanceRR (node ltrim y r)
    | x > y && height (getLeft l) - height (getRight l) < 0
        = rebalanceLL (node l y rtrim)
    | x < y = rebalanceRL (node ltrim y r)
    | x > y = rebalanceLR (node l y rtrim)
    | otherwise = error "avlDelete error, case not possible"
        where rtrimmin = avlDelete rmin r
              ltrim = avlDelete x l
              rtrim = avlDelete x r
              rmin = findMin r

testDelete :: Test
testDelete =
    TestList[avlDelete 3 (node (node (node E 1 E) 2 (node E 3 E)) 5 (node E 10 E))
             ~?= N 1 3 (N 1 2 (N 0 1 E 1 E) 2 E) 5 (N 0 1 E 10 E)]

