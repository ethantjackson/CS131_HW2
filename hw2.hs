-- Q1
-- a
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
scale_nums :: Num b => [b] -> b -> [b]
scale_nums list factor = map (\x -> x*factor) list

-- b
only_odds :: (Foldable t, Integral a) => [t a] -> [t a]
only_odds lists = filter (all (\list -> odd list)) lists

-- c
largest_in_list :: Foldable t => t [Char] -> [Char]
largest_in_list strings = foldl largest "" strings
  where 
    largest s1 s2 = 
      if length s2 > length s1 
        then s2
      else s1

-- Q2
-- a
count_if :: (a -> Bool) -> [a] -> Int
count_if pred_func [] = 0
count_if pred_func (x:xs) = 
  if pred_func x
    then count_if pred_func xs + 1
  else count_if pred_func xs

-- b
count_if_filt :: (a -> Bool) -> [a] -> Int
count_if_filt pred_func list = length (filter pred_func list)

-- c
count_if_fold :: (a -> Bool) -> [a] -> Int
count_if_fold pred_func list = foldl (\acc item -> if pred_func item then acc + 1 else acc) 0 list

-- Q3
-- a
{-
  Currying decomposes a function with n variables into n functions taking only 1 variable each. The first n-1 functions
  return a function, and the final, innermost nth function returns the desired expression. Partial execution involves
  returning using one of these intermediate functions in the currying process, effectively using up to n-1 prefilled/
  default argument values.
-}

-- b
{-
  This is only equivalent to ii. Both func1 and func2 (as show below) can be called as func1 1 2 and func2 1 2. In fact,
  func2's type declaration is syntactic sugar for func1's type declaration; Haskell will automatically curry func2 into func1.
-}
func1 :: Int -> (Int -> Int)
func1 a = \b -> a + b

func2 :: Int -> Int -> Int
func2 a b = a + b

-- c
foo :: (Integer -> (Integer -> (Integer -> ((Integer -> b) -> [b]))))
foo =
  (\x -> (\y -> (\z -> (\t -> map t [x, x+z..y]))))

-- Q4
-- a, b, c
{-
  (1) captures none
  (2) captures b
  (3) captures c, d
-}

-- d
{-
  4, 5, 6, 7 are bound to a, b, e, f.
  f returns 5. f returns a lambda function which take e and f. This lambda function calls c, which returns d, which is a function
  that returns takes e, but only returns a captured b. Thus, b is the only input which affects f's output.
-}

-- Q5
{-
  Haskell closures are first-class citizens, as they can be passed as arguments, used as return values, and can be as assigned as
  variables. Since Haskell closures consist of a function pointer and captured variables, they have the additional advantage of 
  storing the value of captured variables (i.e. their state) at a certain point in time. On the other hand, if C function pointer
  functions refer to a certain variable, they will always use the value of the variable in its current state.
-}

-- Q6
-- a
{-
data InstagramUser = 
  Influencer [String] | 
  Normie
-}

-- b
{-
lit_collab :: InstagramUser -> InstagramUser -> Bool
lit_collab Influencer Influencer = True
lit_collab a b = False
-}

-- c
{-
data InstagramUser = 
  Influencer [String] | 
  Normie
-}

-- d
{-
is_sponsor :: InstagramUser -> String -> Bool
is_sponsor (Influencer sponsors) target = elem target sponsors
is_sponsor _ _ = False
-}

-- e
data InstagramUser = 
  Influencer [String] [InstagramUser] | 
  Normie

-- f
count_influencers :: InstagramUser -> Integer
count_influencers (Influencer sponsors followers) = foldl (\acc follower -> if isInfluencer follower then acc + 1 else acc) 0 followers
  where 
    isInfluencer (Influencer sponsors followers) = True
    isInfluencer _ = False
count_influencers _ = 0

-- g
{-
  The constructor is a curried function, which takes in a String list, then returns a function which takes an InstagramUser list and
  returns an InstagramUser.
-}

-- Q7
data LinkedList = EmptyList | ListNode Integer LinkedList
  deriving Show

-- a
ll_contains :: LinkedList -> Integer -> Bool
ll_contains EmptyList _ = False
ll_contains (ListNode head next) target = (head == target) || ll_contains next target

-- b
{- 
  ll_insert takes:
    1) index :: Integer
    2) value :: Integer
    3) head of the linked list :: LinkedList
  ll_insert returns a new LinkedList head, pointing to new, duplicated nodes up to and
  including index, with nodes following index being the original nodes
-}
ll_insert :: Integer -> Integer -> LinkedList -> LinkedList

-- c
ll_insert index value EmptyList = ListNode value EmptyList 
ll_insert index value node
  | index <= 0 = ListNode value node
  | otherwise = ListNode (getVal node) (ll_insert (index-1) value (getnext node))
      where
        getVal (ListNode val next) = val
        getnext (ListNode val next) = next

-- Q8 
-- a
{-
  #include <vector>
  using namespace std;

  int longestRun(vector<bool> v) {
    int longest = 0;
    int cur = 0;
    for (bool b : v) {
      if (b) {
        cur += 1;
      } else {
        longest = std::max(cur, longest);
        cur = 0;
      }
    }
    return longest;
  }
-}
