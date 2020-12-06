#lang racket/base

(require racket/file
         racket/list
         racket/match
         racket/string)

(define (day4a file)
  (for/sum ([passport (string-split (file->string file) "\n\n")])
    (let ([parts (flatten
                  (for/list ([field (string-split passport #rx"[ \n]")])
                    (string-split field ":")))])
      (if (valid-passport? parts) 1 0))))

(define (valid-passport? parts)
  (let ([h (apply hash parts)])
    (and (valid-birth-year? (hash-ref h "byr" #f))
         (valid-issue-year? (hash-ref h "iyr" #f))
         (valid-expiration-year? (hash-ref h "eyr" #f))
         (valid-height? (hash-ref h "hgt" #f))
         (valid-hair-color? (hash-ref h "hcl" #f))
         (valid-eye-color? (hash-ref h "ecl" #f))
         (valid-passport-id? (hash-ref h "pid" #f)))))

(define (number-between? v min max)
  (and (>= v min) (<= v max)))

(define (valid-birth-year? v)
  (and v (number-between? (string->number v) 1920 2002)))

(define (valid-issue-year? v)
  (and v (number-between? (string->number v) 2010 2020)))

(define (valid-expiration-year? v)
  (and v (number-between? (string->number v) 2020 2030)))

(define (valid-height? v)
  (and v (match (string->list v)
           [(list n ... #\c #\m) (number-between? (string->number (list->string n)) 150 193)]
           [(list n ... #\i #\n) (number-between? (string->number (list->string n)) 59 76)]
           [else #f])))

(define (valid-hair-color? v)
  (and v (regexp-match-exact? #px"#[0-9a-z]{6}" v)))

(define (valid-eye-color? v)
  (and v (regexp-match-exact? #rx"amb|blu|brn|gry|grn|hzl|oth" v)))

(define (valid-passport-id? v)
  (and v (regexp-match-exact? #px"[0-9]{9}" v)))

(day4a "day4-input.txt")