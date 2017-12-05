input = 325489.0

distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

distanceCenter = distance (0,0)

side n = realToFrac $ floor ((n-((s-1)^2)) / s)
    where s = 2 * ring n

ring n = realToFrac $ floor ((sqrt n + 1) / 2)

pos n = (n - ((2 * r) -1) ^ 2) - (2 * r * s) - r + 1
    where r = ring n
          s = side n

x n = abs $ case side n of
    0 -> ring n
    1 -> -pos n
    2 -> -ring n
    3 -> pos n

y n =  abs $ case side n of
    0 -> pos n
    1 -> ring n
    2 -> -pos n
    3 -> -ring n

answer n = distanceCenter (x n', y n')
    where n' = n - 1

-- 3b gjord genom manuell räkning. Inte snyggt, men funkar.