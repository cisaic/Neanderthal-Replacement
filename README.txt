README

ADDITIONS TO CODE:
- defvar *state0*
- defvar *state1*
- defvar *adv0*
- defvar *adv1*
- defun calcBias
- defun difNeighborsp
- defun alldifNeighborsp
- defun printpopulation

MODIFICATIONS TO CODE: 
- neighbors & neighbors-types -> neighborsf
- death-birth1 & death-birth2 -> replacement
- run1
- run

RUNNING INSTRUCTIONS:
(run n nx x advx ny y advy bias)

n = number of replications of the simulation
nx = number of x’s
x = symbol representation of population x
advx = costal advantage for x (-1, 0, or 1)
ny = number of y’s
y = symbol representation of population y
advx = costal advantage for y (-1, 0, or 1)
bias = 0 <= bias <= 1.0 in favour of y

NOTE: the value of nx + ny must be a perfect square

ex. (run 20 33 0 0 67 1 1 0.52)
run 20 replications of the following simulation:
start with 33 1’s and 67 0’s. The 0’s don’t have any advantage at the coast, but the 1’s do. The bias in favor of 1’s is 0.52.

Description of the following files:

1-33-0-0-67-1-0-067.mov
	Visual animation of 1 replication of 10x10 population consisting of 33 Neanderthals (0) and 67 Moderns (1), with a 0.67 bias in favour of Neanderthals. Neither population has any advantage at the coast

1-33-0--1-67-1-1-067.mov
	Visual animation with the same initial parameters as above, except Neanderthals have a disadvantage at costal interactions (-1), and Moderns have an advantage (1)



