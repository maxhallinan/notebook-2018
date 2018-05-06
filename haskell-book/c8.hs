module C8 where

applyTimes 0 f b = b
applyTimes n f b = applyTimes (n-1) f (f b)

-- applyTimes 5 (+1) 5 = applyTimes (5 - 1) (+1) ((+1) 5)
-- applyTimes 4 (+1) 6 = applyTimes (4 - 1) (+1) ((+1) 6)
-- applyTimes 3 (+1) 7 = applyTimes (3 - 1) (+1) ((+1) 7)
-- applyTimes 2 (+1) 8 = applyTimes (2 - 1) (+1) ((+1) 8)
-- applyTimes 1 (+1) 9 = applyTimes (1 - 1) (+1) ((+1) 9)
-- applyTimes 0 (+1) 10 = 10
-- (applyTimes 0 (+1) (applyTimes 1 (+1) (applyTimes 2 (+1) (applyTimes 3 (+1) (applyTimes 4 (+1) 6)))))
--

