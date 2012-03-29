-- | Some convenient debugging facilities.
--
-- Say you want to debug a function @f@ defined by
-- > f x = x' where ...
-- This is easily done by inserting a 'tr' or 'tr'' in a guard:
-- > f x
--     | tr "f was called" = undefined
--     | otherwise         = x' where ...
--
-- Several 'tr' and 'tr'' may also be combined by using '||':
-- > f x
--     | tr "f was called with argument:" || tr' x = undefined
--     | otherwise         = x' where ...
--
-- @f@ produces the same result as before, because 'tr' and 'tr''
-- always return 'False' (since 'Debug.Trace.trace is used, the
-- compiler won't optimize this away).

module Misc.GuardTrace where

import Debug.Trace

tr :: String -> Bool
tr s = Debug.Trace.trace s False

-- | A shorthand for undefined.
rt :: a
rt = undefined

tr' :: (Show a) => a -> Bool
tr' = tr . show

trace :: String -> a -> a
trace = Debug.Trace.trace

traceShow :: (Show a) => a -> b -> b
traceShow = Debug.Trace.traceShow