#lang racket

(provide (all-defined-out))

(define elementary-knightship
  "#N Sir Robin
#O Adam P. Goucher, Tom Rokicki; 2018
#C The first elementary knightship to be found in Conway's Game of Life.
#C http://conwaylife.com/wiki/Sir_Robin
x = 31, y = 79, rule = B3/S23
4b2o$4bo2bo$4bo3bo$6b3o$2b2o6b4o$2bob2o4b4o$bo4bo6b3o$2b4o4b2o3bo$o9b
2o$bo3bo$6b3o2b2o2bo$2b2o7bo4bo$13bob2o$10b2o6bo$11b2ob3obo$10b2o3bo2b
o$10bobo2b2o$10bo2bobobo$10b3o6bo$11bobobo3bo$14b2obobo$11bo6b3o2$11bo
9bo$11bo3bo6bo$12bo5b5o$12b3o$16b2o$13b3o2bo$11bob3obo$10bo3bo2bo$11bo
4b2ob3o$13b4obo4b2o$13bob4o4b2o$19bo$20bo2b2o$20b2o$21b5o$25b2o$19b3o
6bo$20bobo3bobo$19bo3bo3bo$19bo3b2o$18bo6bob3o$19b2o3bo3b2o$20b4o2bo2b
o$22b2o3bo$21bo$21b2obo$20bo$19b5o$19bo4bo$18b3ob3o$18bob5o$18bo$20bo$
16bo4b4o$20b4ob2o$17b3o4bo$24bobo$28bo$24bo2b2o$25b3o$22b2o$21b3o5bo$
24b2o2bobo$21bo2b3obobo$22b2obo2bo$24bobo2b2o$26b2o$22b3o4bo$22b3o4bo$
23b2o3b3o$24b2ob2o$25b2o$25bo2$24b2o$26bo!")

(define schick-engine
  "#N Schick engine
#O Paul Schick
#C An orthogonal period 12 c/2 tagalong found in 1972.
#C www.conwaylife.com/wiki/index.php?title=Schick_engine
x = 20, y = 11, rule = 23/3
bo2bo15b
$o19b$o3bo15b
$4o9b2o5b
$6b3o5b2o4b
$6b2ob2o6b3o
$6b3o5b2o4b
$4o9b2o5b
2o5b
$o3bo15b
$o19b
$bo2bo!")

(define simple-glider
  "#N Glider
#O Richard K. Guy
#C The smallest, most common, and first discovered spaceship. Diagonal, has period 4 and speed c/4.
#C www.conwaylife.com/wiki/index.php?title=Glider
x = 3, y = 3, rule = B3/S23
bob$2bo$3o!")

(define pufferfish
  "#N Pufferfish
#O Richard Schank
#C An almost natural c/2 puffer, discovered in November 2014.
#C www.conwaylife.com/wiki/Pufferfish
x = 15, y = 12, rule = B3/S23
3bo7bo$2b3o5b3o$b2o2bo3bo2b2o$3b3o3b3o2$4bo5bo$2bo2bo3bo2bo$o5bobo5bo$
2o4bobo4b2o$6bobo$3bobo3bobo$4bo5bo!")

(define copperhead
  "#N Copperhead
#O 'zdr'
#C An c/10 orthogonal spaceship found on March 5, 2016.
#C http://www.conwaylife.com/wiki/Copperhead
x = 8, y = 12, rule = B3/S23
b2o2b2o$3b2o$3b2o$obo2bobo$o6bo2$o6bo$b2o2b2o$2b4o2$3b2o$3b2o!")

(define gosper
  "#N Gosper glider gun
#O Bill Gosper
#C A true period 30 glider gun.
#C The first known gun and the first known finite pattern with unbounded growth.
#C www.conwaylife.com/wiki/index.php?title=Gosper_glider_gun
x = 36, y = 9, rule = B3/S23
24bo11b$22bobo11b$12b2o6b2o12b2o$11bo3bo4b2o12b2o$2o8bo5bo3b2o14b$2o8b
o3bob2o4bobo11b$10bo5bo7bo11b$11bo3bo20b$12b2o!")