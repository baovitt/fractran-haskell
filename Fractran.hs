first :: [a] -> (a -> Bool) -> Maybe a
first list conditional
    | null list = Nothing
    | conditional (head list) = Just (head list)
    | otherwise = first (tail list) conditional

fractran :: (Integral a) => [(a, a)] -> a -> a -> Maybe a
fractran fractions input iterations
    | null fractions = Nothing
    | input < 1 = Nothing
    | iterations == 0 = Just input
    | otherwise = newInput >>= (\(num, den) -> 
        fractran fractions (div (input * num) den) (pred iterations))
    where 
        newInput = first fractions (\(num, den) -> 
            if mod (input * num) den == 0 then True else False)