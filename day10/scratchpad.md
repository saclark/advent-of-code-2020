# Notes
* Given '(a b c), '(b) can only be omitted if (<= (+ (- b a) (- c b)) 3)
* Given '(a b c d), '(b c) can only be omitted if (<= (+ (- b a) (- c b) (- d c)) 3)
* At most, only two consecutive adapters can be omitted. Three adapters in a row can never be omitted

# Test 1 (8)

nums: (0) 1 4 5 6 7 10 11 12 15 16 19 (22)
diff:     1 3 1 1 1 3  1  1  3  1  3  3
skip:         x x      x

x
5
6
11
5 6
5 11
6 11
5 6 11


(0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
(0), 1, 4, 7, 10, 12, 15, 16, 19, (22)



# Test 2 (19208)

nums: (0) 1 2 3 4 7 8 9 10 11 14 17 18 19 20 23 24 25 28 31 32 33 34 35 38 39 42 45 46 47 48 49 (52)
diff:     1 1 1 1 3 1 1 1  1  3  3  1  1  1  3  1  1  3  3  1  1  1  1  3  1  3  3  1  1  1  1  3
skip:     x x x     x x x           x  x        x           x  x  x                 x  x  x


* find contiguous lists of removable numbers, calculate their permutations, multiply together
* group terminates when two consecutive diffs sum to > 3

> (* 7 7 7 7 4 2)
19208

12 = 2
112 = 3 = 3 4 5 7
121 = 3  = 3 4 6 7

11 = 2
111 = 4
1111 = 7
11111 = 13
111111 = 24

# Scratch

(0) 1 2 3 4 5 (6)
    1 1 1 1 1 1
    x x x x x
    x x x x
    

(0) 1 3 4 5 6 9
    1 2 1 1 1 3
    x x x x
        x x