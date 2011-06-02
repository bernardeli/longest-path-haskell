-- HOWTO:
-- ghci
-- :load longest-path.hs
-- longestpath tree

-- tree structure
data T = F Int | N Int T T

-- some example
tree = (N 1
          (N 2
              (N 3
                  (F 4)
                  (N 5
                      (F 6)
                      (N 7
                          (F 8)
                          (N 9
                              (F 10) (F 11)))))
              (N 12
                  (F 13)
                  (N 14
                      (F 15)
                      (N 16
                          (F 17)
                          (N 18
                              (F 19) (F 20))))))
          (N 21
              (F 22) (F 23)))

-- checks if this is a leaf
leaf (F _) = True
leaf _ = False

-- returns the value aggregated on leaf or node
value (F e) = e
value (N e _ _) = e

-- gets second and third element
secondpositiontuple (_, e, _) = e
thirdpositiontuple (_, _, e) = e

-- gets left side and right side
getleft (N _ te _) = te
getright (N _ _ td) = td

-- calculates tree's depth

longestpath (F e) = [e]
longestpath tree = calculatepath tree []

calculatepath (F _) acc = acc
calculatepath tree acc
                | (size > leftsize) && (size > rightsize) = path
                | otherwise =
                    if leftsize > rightsize then
                        leftpath
                    else
                        rightpath
                    where path = (reverse (thirdpositiontuple (getdown (getleft tree))))++[(value tree)]++(thirdpositiontuple (getdown (getright tree)))
                          leftpath = calculatepath (getleft tree) path
                          rightpath = calculatepath (getright tree) path
                          size = getsize path
                          leftsize = getsize leftpath
                          rightsize = getsize rightpath

getsize [] = 0
getsize (a:x) = 1 + (getsize x)

getdown tree = getdowntree tree 0 []

getdowntree (F e) acc list = ((F e), acc, (list ++ [e]))
getdowntree node acc list
            | leaf (getleft node) && leaf (getright node) = getdowntree (getright node) (acc+1) (list ++ [(value node)])
            | leaf (getleft node) = getdowntree (getright node) (acc+1) (list ++ [(value node)])
            | leaf (getright node) = getdowntree (getleft node) (acc+1) (list ++ [(value node)])
            | otherwise = 
                if secondpositiontuple left > secondpositiontuple right then 
                    left
                else 
                    right
            where left = getdowntree (getright node) (acc + 1) (list++[(value node)])
                  right = getdowntree (getleft node) (acc + 1) (list++[(value node)])
