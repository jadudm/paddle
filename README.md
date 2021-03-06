paddle is a *microworld*. This means that Paddle lets you simulate the world around you. However, the world is a very big place. Therefore, it is best to think of paddle as a set of tools for simulating a small part of the world: you're going to have to make some assumptions, approximate here and there, and in the end, you'll have a model of the world that lets you explore some idea.

paddle is inspired by [NetLogo](https://ccl.northwestern.edu/netlogo/) and its predecessor [StarLogo](https://education.mit.edu/project/starlogo-tng/).

## Seeing paddle in action

You can check some videos out on YouTube to see paddle in action.

* In this first video, at [1:52](https://youtu.be/kCw9F180g8k?t=112), you can see a simulation of particle diffusion. This is arguably what paddle was developed for.
* At [38 seconds in](https://youtu.be/FpMy9QxWnhE?t=38) on this second video, you can see the simulation of termites picking up woodchips.
* At [2:02](https://youtu.be/FpMy9QxWnhE?t=122) you can see a simulation of boids flocking.
* At [3:04](https://youtu.be/FpMy9QxWnhE?t=184) is Conway's *Game of Life*.
* Finally, I also experimented with interactive GUIs, a prototype of which can be seen [here](https://youtu.be/HniGoLgyyuk?t=28).


## Installing paddle

First, install [DrRacket](). This is a programming environment for the language Racket, a descendant of Scheme. 

To install paddle, go to the *File* menu in DrRacket, then the *Package Manager*. Type **paddle** and click the *Install* button. 

## Exploring paddle

There is [some documentation](https://docs.racket-lang.org/paddle@paddle/index.html) written, but paddle was a technology exploration carried out between myself and students. For me, it was a chance to explore macros, the implementation of a small language, and drawing interesting things in OpenGL. (And, I got to implement a [quadtree](https://github.com/jadudm/paddle/blob/explore/src/quadtree.rkt), with some rudimentary testing no less.) The ultimate goal was to use it as a simulation environment as part of a research-based course for first-year students studying particulates and air quality.

To explore paddle on your own, you'll want to [check out the source](https://github.com/jadudm/paddle/tree/master/examples). You can either clone the repository, or download it as a zipfile. Then, using DrRacket, open the code examples in the `examples` directory. 

* The [termites](https://github.com/jadudm/paddle/blob/master/examples/termites.rkt) example is one of my favorites. It demonstrates centralized behavior from a decentralized algorithm. So very cool.
* The [boids](https://github.com/jadudm/paddle/blob/master/examples/boids.rkt) example implements a basic version of [Reynold's boids](https://en.wikipedia.org/wiki/Boids) simulation. 
* The [sheep and wolves](https://github.com/jadudm/paddle/blob/master/examples/sheep-wolves.rkt) simulation includes an experiment in live plotting. Note that this simulation never settles. By this, the wovles eventually die, and the sheep's population explodes. I recommend stopping the simulation at this point, as I have nothing in place to constrain the runaway. Your computer will slow down.
* Some of my explorations into performance considerations were driven by a desire to be able to support [Conway's game of life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) simulations. To explore it, open [gameoflife.rkt](https://github.com/jadudm/paddle/blob/master/examples/gameoflife.rkt) and follow the instructions. I recommend, as a start, hitting *Run* first, and then pasting `(run-random 100 600)` into the interactions area. After that, perhaps `(run-pattern 'puffer-1 100 600)`. Note that this pulls a pattern from [http://www.conwaylife.com/](http://www.conwaylife.com/), parses it, and runs it, so a network connection is necessary.

## New worlds in paddle

The documentation is sparse, because this was a "robust prototype." My goal was to develop it further for the 2019--2020 academic year. (That is, there were several more months of work yet to be done.) The context was going to be a course that investigated air quality, and we would use paddle to conduct some *in silico* simulations of diffusion to augment our *in vivo* experiments with particulate counters. 

So, it is possible to use paddle for your own explorations, and the documentation that exists is accurate, but the full language and environment that paddle represents is not yet fully documented. 

## Open Source

paddle is free and open software, and will be a place I'll go back to again for future exploration.