# Notes for a paper about generic parallel functional programming

## Outline

*   Introduction
*   Generic programming in Haskell
*   Some useful data types:
    *   Right-lists
    *   Left-lists
    *   Top-down trees
    *   Bottom-up trees
    *   Shape-indexed variations
    *   Top-down and bottom-up bushes
*   Parallel scan
*   FFT
*   Related work
*   Reflections/conclusions

## Abstract

Parallel programming, whether imperative or functional, has long focused on arrays as the central data type.
Meanwhile, typed functional programming has explored a variety of data types, including lists and various forms of trees.
*Generic* functional programming decomposes these data types into a small set of fundamental building blocks: sum, product, composition, and their associated identities.
Definitions over these few fundamental type constructions then automatically assemble into algorithms for an infinite set of data types---some familiar and some new.
This paper presents generic functional formulations for two important and well-known classes of parallel algorithms: parallel scan (generalized prefix sum) and Fast Fourier Transform (FFT).
Notably, arrays play no role in these formulations.
Consequent benefits include a simpler and more compositional style, much use of common algebraic patterns (such as `Functor`, `Applicative`, `Foldable`, and `Traversable`), and freedom from possibility of run-time indexing errors.
The functional generic style also clearly reveals deep commonality among what otherwise appears to be quite different algorithms.
Instantiating the generic formulations to "top-down" and "bottom-up" trees, two well-known algorithms for each of parallel scan and FFT naturally emerge, as well as two possibly new algorithms.

## Introduction

## Generic programming in Haskell

## Some useful data types:

### Right-lists

### Left-lists

### Top-down trees

### Bottom-up trees

### Shape-indexed variations

### Top-down and bottom-up bushes

## Parallel scan

## FFT

## Related work

## Reflections/conclusions

* * * * * * * * * * * * * * * * * * * *

## Misc

*   Alternative to arrays:
    *   More compositional.
    *   Exploit operations from existing abstractions (type classes).
    *   Safe from numeric indexing errors.
*   Infinite family of correct algorithms indexed by data type.
    It's easier to select a data type than implement a correct parallel algorithm.
*   "Parallel-friendly" might be more suitable than "parallel".
*   Reveals commonality of some algorithms that appear quite different.
    Theme: postpone optimization.
