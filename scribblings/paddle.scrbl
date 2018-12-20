#lang scribble/manual
 
@title[]{Paddle}

@emph{A paddle is a little like a racket}.
 
@;==========================================
@section{What is Paddle?}

Paddle is a @emph{microworld}. This means that Paddle lets you simulate the world around you. However, the world is a very big place. Therefore, it is best to think of Paddle as a set of tools for simulating a small part of the world: you're going to have to make some assumptions, approximate here and there, and in the end, you'll have a model of the world that lets you explore some idea.

Paddle is not the first microworld, nor is it the only. It is inspired heavily by @link["https://ccl.northwestern.edu/netlogo/"]{NetLogo}. NetLogo provides a way to write code for simulating everything from intrepid turtles to flocking birds to viral epidemics. Paddle borrows language and ideas from NetLogo (for consistency) and implements them in Racket.

@;==========================================
@section{A Complete Example}

Here is an example of a paddle that randomly distributes turtles around the world, and then sets them moving in a straight line.


@codeblock|{

#lang racket
(require paddle)
(make-world 100)

(create-breed turtle turtles)
(create turtles 100)

(define (setup)
  (ask turtles
      (set xcor (random (get global world-cols)))
      (set ycor (random (get global world-rows)))
      (set direction (random 360))
  ))

(define (go)
  (ask turtles
    (move 1))
  )


(tick-pause (/ 1 30))
(run-world setup go)
}|


What follows is a brief walkthrough of the example.

@;----------------------------------------
@subsection{Setting up the world}

If you want to create a Paddle world, you need to first require the @racketidfont{paddle} package.

@racketblock[(require paddle)]

Next, we need to create the world. The world is made up of @emph{patches}, which can be thought of like a checkerboard. Turtles wander over these patches, and can (if they wish) interact with those patches.

@racketblock[(make-world 100)]

@racketidfont{make-world} is a function that, in its simplest form, takes one parameter. This defines the number of columns and rows of patches in the world---or, the @emph{x} and @emph{y} dimensions of the world. With this command, I've said I want a world with 100x100 patches, and it will be displayed in a 600x600 window.

@margin-note{
  In microworlds, simulated creatures are most often referred to as @emph{agents}. In NetLogo, they often refer to them as @emph{turtles}; this is because, in the programming language @link["http://el.media.mit.edu/logo-foundation/index.html"]{Logo}, the "turtle" plays a prominent role (and NetLogo is descended from Logo). I personally like the idea of turtles everywhere, so I will generally use the word @emph{turtles} to mean @emph{computational agents that wander around and interact with the world and other agents in a simulated environment}. 
}

Next, we need to declare a new breed of agents in our microworld; I'll choose to declare my agents to be turtles. I need to give both the singular and plural form for this new breed of agent. I then create 100 turtles in the world.

@racketblock[
  (create-breed turtle turtles)
  (create turtles 100)
]

@;----------------------------------------
@subsection{Setting Up and Running the World}

To run our microworld, we need to provide two functions: @racketidfont{setup} and @racketidfont{go}. The @racketidfont{setup} function is used once when our world is created, and @racketidfont{go} is used over-and-over. Inside the @racketidfont{go} function, we wiggle and move our turtles, and after the function is run, the world is redrawn to reflect the changes in our turtles and the world around them.

@racketblock[

(define (setup)
  (ask turtles
      (set xcor (random (get global world-cols)))
      (set ycor (random (get global world-rows)))
      (set direction (random 360))
  ))

(define (go)
  (ask turtles
    (move 1))
  )

(tick-pause (/ 1 30))
(run-world setup go)
]
@margin-note{The @racketidfont{set} and @racketidfont{get} forms are borrowed from NetLogo. @racketidfont{set} will set a value for the current agent that is being @racketidfont{ask}ed to do things. Likewise, when we say @racketidfont{get}, we will retrieve a value from the current agent. It is possible to set and retrieve values for a @racketidfont{patch} or for the @racketidfont{global} environment as well.}

The @racketidfont{setup} function @racketidfont{asks} all of the turtles to do three things. First, we set the @emph{xcor}, or x-coordinate, of each turtle to a @racket[random] value between zero and the number of columns in the world. Then, we randomize the y-coordinate in the same way; this has the effect of scattering turtles around the world. Lastly, I set the turtle's direction so each turtle has a randomized heading.

In @racketidfont{go}, I @racketidfont{ask} turtles to move forward one patch. (In a world that is 100x100, a move of 1 is 1/100th of the distance across the world.)

Finally, the @racketidfont{setup} and @racketidfont{go} functions are handed off to the @racketidfont{run-world} function, which then does a bunch of work behind the scenes. Along the way, it runs the @racketidfont{setup} function once, and @racketidfont{go} function over-and-over, with a 1/30th of a second pause between runs, which is determined by @racketidfont{tick-pause}; leaving out the pause will let the simulation run as fast as possible.

@section{To Be Continued}

This package is under active development. Conversation and collaboration are welcome. Next steps include integrating plotting and logging, additional interface forms, and completing documentation. (Documentation may come later, as I'm still exploring the system implementation itself.)

Optimization will come; a quadtree or similar for agents (so as to optimize @racketidfont{sniff}) is a likely next step. Likewise, thinking about how the code is broken across forms, and deciding how best to test this world is also important for maintainability.
