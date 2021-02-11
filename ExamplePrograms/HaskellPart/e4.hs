--function definition
--example 4 - combining functions


sq x = x ^ 2
times2 x = 2 * x

--sqTimes2 x = times2 sq x
--above does not work

--sqTimes2 x = times2 (sq x)
-- above works, below shows function composition

sqTimes2 = times2 . sq
