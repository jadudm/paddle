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
(make-world 100 600)

(create-breed turtle turtles)
(create turtles 100)

(define (setup)
  (ask turtles
      (set turtle-x (random (get global world-columns)))
      (set turtle-y (random (get global world-rows)))
      (set turtle-direction (random 360))
  ))

(define (go)
  (ask turtles
    (move 1))
  ;; Slow the turtles down!
  (sleep (/ 1 20))
  )

(run-world setup go)
}|


What follows is a brief walkthrough of the example.

@;----------------------------------------
@subsection{Setting up the world}

If you want to create a Paddle world, you need to first require the @racketidfont{paddle} package.

@racketblock[(require paddle)]

Next, we need to create the world. The world is made up of @emph{patches}, which can be thought of like a checkerboard. Turtles wander over these patches, and can (if they wish) interact with those patches.

@racketblock[(make-world 100 600)]

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
      (set xcor (random (get global world-columns)))
      (set ycor (random (get global world-rows)))
      (set direction (random 360))
  ))

(define (go)
  (ask turtles
    (move 1))
  (sleep (/ 1 20))
  )

(run-world setup go)
]
@margin-note{The @racketidfont{set} and @racketidfont{get} forms are borrowed from NetLogo. @racketidfont{set} will set a value for the current agent that is being @racketidfont{ask}ed to do things. Likewise, when we say @racketidfont{get}, we will retrieve a value from the current agent. It is possible to set and retrieve values for a @racketidfont{patch} or for the @racketidfont{global} environment as well.}

The @racketidfont{setup} function @racketidfont{asks} all of the turtles to do three things. First, we set the @emph{xcor}, or x-coordinate, of each turtle to a @racket[random] value between zero and the number of columns in the world. Then, we randomize the y-coordinate in the same way; this has the effect of scattering turtles around the world. Lastly, I set the turtle's direction so each turtle has a randomized heading.

In @racketidfont{go}, I @racketidfont{ask} turtles to move forward one patch. (In a world that is 100x100, a move of 1 is 1/100th of the distance across the world.)

Finally, the @racketidfont{setup} and @racketidfont{go} functions are handed off to the @racketidfont{run-world} function, which then does a bunch of work behind the scenes. Along the way, it runs the @racketidfont{setup} function once, and @racketidfont{go} function over-and-over, with a 1/20th of a second pause between frames, which is determined by @racketidfont{sleep}; leaving out the pause will let the simulation run as fast as possible.

@section{Paddle: World}

There are several functions and syntaxes that are required to set up a paddle microworld. 

@subsection{@italic{make-world}}

One of the first functions we need is the one that defines the dimensions of the microworld.

@defproc[(make-world 
           [world-rows-by-columns number?]
           [frame-rows-by-columns number?])
         _]{ 
         Sets the dimensions of a paddle microworld.
         }

One of the first things a paddle program needs to do is define the dimensions of the world. All paddle worlds are a grid of squares: think of a checkerboard, or graphc paper... the @italic{patches} of the world are the squares, and the @italic{agents} of the world move from one patch to the next. 

There are two different dimensions that a paddle programmer needs to specify:

@itemlist[
  @item{@italic{world-rows-by-columns}: This parameter is the number of grid squares across and down in the world. For example, if you wanted a a world that has 100 squares, it would be 10x10, so this parameter would be @racket[10].}
  @item{@italic{frame-rows-by-columns}: This parameter is the number of pixels, on the screen, that the world will be displayed as. So, if you want your 100x100 world to be displaced at a 1:1 ratio, this value should also be 10. However, this would be @italic{tiny}! On modern displays, a value of 400, 600, or 800 is a good starting point.}]
  
If you are uncertain what values to begin with, try starting with a world that is 100x100 and displayed at 400x400. This would suggest

@racketblock[(make-world 100 400)]

Because it is sometimes useful to refer back to the world dimensions, you might want to define a variable to contain this value.

@racketblock[
(define RxC 100)
(make-world RxC 400)
]

@subsection{@italic{create-breed} and @italic{create}}

Once we have defined our world, we need agents to populate it. In NetLogo, agents are thought of as belonging to a @italic{breed}. So, we borrow this concept.

@defproc[#:kind "syntax"
  (create-breed 
    [singular identifier]
    [plural identifier]) _]{ 
         Defines a breed of agent.
         }

For example, in my home town, we might want to have a breed of agents that we will refer to as @italic{bobcats}. I would define this breed as follows:

@racketblock[
  (create-breed bobcat bobcats)
]

While it is true that we could automatically guess the plural, it is easier to let the programmer decide what the singular and plural form of their breed will be. These forms show up throughout a paddle program.

@defproc[
  (create
    [breed-plural identifier]
    [number number?]) _]{ 
         Creates a given number of agents of the given breed.
         }

This procedure creates a number of agents in the microworld of the given breed. These agents are given a set of default values that the programmer may want to modify later (see @secref{setup}). 

If I want one bobcat in the world, I would write

@racketblock[
(create bobcats 1)
]

If I want 1000 bobcats, I write 

@racketblock[
(create bobcats 1000)
]

It is possible to have different breeds in the world at the same time. For example, bobcats are natural rivals of polar bears (in Maine, anyway), and they often compete for scarce natural resources in the region (snow shovels and kayaks). Declaring and creating both of these breeds of agent might look like

@racketblock[
(create-breed bobcat bobcats)
(create-breed polar-bear polar-bears)

(create bobcats 1000)
(create polar-bears 1000)
]

None of these agents are visible in the world until we run it.

@section[#:tag "setup"]{Setting up the world}

There are two functions needed to execute a microworld: an initial setup function (which is executed once) and then a run function (which is run repeatedly and indefinitely). As a programmer, you are free to name these anything you want, but by convention, they will be referred to here as @italic{setup} and @italic{go}.



@section{To Be Continued}

This package is under active development. Conversation and collaboration are welcome. Next steps include integrating plotting and logging, additional interface forms, and completing documentation. As of January 1, 2019, things are stabilizing, and documentation will follow. Currently, I am implementing examples so as to find where the environment does, and does not, work as a microworld.

I am currently making notes and tracking "to do" type items through a combination of the GitHub ticket queue and a @link["https://trello.com/b/BYmiCIyl/paddle"]{Trello board}. If you're interested in contributing (examples, core language/API, optimizations), please feel free to reach out via email, make a pull request, etc. 