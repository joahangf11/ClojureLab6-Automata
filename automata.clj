;----------------------------------------------------------
; Problem Set #6: Automata
; Date: April 28, 2023.
; Authors:
;          A01748222 Joahan Javier Garcia Fernandez
;          A01747811 Benjamín Alejandro Cruz Cervantes
;          A01754574 Luis Fernando De León Silva
;----------------------------------------------------------

;Namespace
(ns automata
  (:require [clojure.test :refer [deftest is run-tests]]))


;definir un nuevo tipo de registro (record type) con campos específicos. Los registros son similares a las clases, pero están diseñados para ser inmutables, lo que los hace ideales para modelar datos en un entorno funcional.
(defrecord DFA [initial-state accept-states transitions])


; #1
; :keyword #set {}map
(def dfa-1 (->DFA :q0 #{:q2} {:q0 {\a :q1
                                   \b :q0}
                              :q1 {\a :q1
                                   \b :q2}
                              :q2 {\a :q2
                                   \b :q2}}))

; #2







;Function

(defn accepts?
  [dfa input]
  (let [initial-state (.initial_state dfa)
        accept-states (.accept_states dfa)
        transitions (.transitions dfa)]
    (loop [input (seq input)
           current initial-state]
      (if (empty? input)
        (contains? accept-states current)
        (recur (rest input)
               ((transitions current) (first input)))))))


;Tests

(deftest test-problem1
  (is (accepts? dfa-1 "ab"))
  (is (accepts? dfa-1 "abba"))
  (is (accepts? dfa-1 "aaab"))
  (is (accepts? dfa-1 "abbbbbbbbb"))
  (is (not (accepts? dfa-1 "")))
  (is (not (accepts? dfa-1 "a")))
  (is (not (accepts? dfa-1 "baa")))
  (is (not (accepts? dfa-1 "bbba"))))


;Run tests

(run-tests)
