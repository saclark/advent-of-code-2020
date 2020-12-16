#lang racket/base

(require racket/file
         racket/list
         racket/string
         rackunit)

(define (solve file)
  (for/sum ([passport (string-split (file->string file) "\n\n")])
    (let ([parts (flatten
                  (for/list ([field (string-split passport #rx"[ \n]")])
                    (string-split field ":")))])
      (if (valid-passport? parts) 1 0))))

(define (valid-passport? parts)
  (let ([h (apply hash parts)])
    (and (hash-has-key? h "byr")
         (hash-has-key? h "iyr")
         (hash-has-key? h "eyr")
         (hash-has-key? h "hgt")
         (hash-has-key? h "hcl")
         (hash-has-key? h "ecl")
         (hash-has-key? h "pid"))))

(check-eqv? (solve "input.txt") 182)