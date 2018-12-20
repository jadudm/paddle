#lang scribble/manual
 
@title{Paddle}

@emph{A paddle is a little like a racket}.

@section{What is paddle?}

paddle is a @emph{microworld}. Put simply, this means that paddle lets you simulate the world around you. However, the world is a very big place. Therefore, it is best to think of paddle as a set of tools for simulating a small part of the world: you're going to have to make some assumptions, approximate here and there, and in the end, you'll have a model of the world that lets you explore some idea.


paddle is not the first microworld, nor is it the only. It is inspired heavily by @link["https://ccl.northwestern.edu/netlogo/"]{NetLogo}. NetLogo provides a way to write code for @emph{turtles}. A turtle can wander around, sniff for other turtles, color the patches of the world it lives in... generally, the turtle can move, sense, and interact with the (simulated) world. However, unlike the original @link["http://el.media.mit.edu/logo-foundation/index.html"]{Logo}---a language that allowed the user to draw geometric pictures with a single turtle---NetLogo makes it easy to write code for tens, hundreds, or even thousands of turtles. In NetLogo write the code for one, and then ask all of the turtles to do the same thing.

paddle borrows the ideas of NetLogo (for consistency with a long-standing language) and implements them in Racket.

@section{An Example: Wandering Turtles}

This example walks through the creation of a simple paddle world.

@subsection{Creating the World}

If you want to create a paddle world, you need to first require the paddle package.

@racketblock[(require paddle)]

Next, we need to create the world. The world is made up of @emph{patches}, which can be thought of like a checkerboard. Our turtles wander around over these patches, and can (if they wish) interact with those patches.

@racketblock[(make-world 100)]

make-world is a function that, in its simplest form, takes one parameter. This defines the number of columns and rows of patches in the world---or, the @emph{x} and @emph{y} dimensions of the world. With this command, I've said I want a world with 100x100 patches, and it will be displayed in a 600x600 window.

@subsection{Defining the turtles}

In microworlds, we generally refer to the simulated creatures as @emph{agents}. In NetLogo, they refer to these as @emph{turtles}. I personally like the idea of lots of little turtles wandering everywhere, so I will generally use the word @emph{turtles} to mean @emph{agents that can wander around and interact with other agents in a simulated world}. 

However, we do need to define turtles as a breed of agent. We do this with create-breed.

@racketblock[
  (create-breed turtle turtles)
  (create turtles 100)
]

create-breed takes the singular and plural forms of our new breed of agents. Once you've defined a breed of agent in the world, you can use the plural form to create some turtles. Here, I've asked for 100 turtles in my world.

@subsection{setup and go}

To run our microworld, we need to provide two functions: setup and go. The setup function is used once when our world is created, and go is used over-and-over. Inside the go function, we wiggle and move our turtles, and after the function is run, the world is redrawn to reflect the changes in our turtles and the world around them.

I'll start with two functions that do nothing.

@racketblock[
  (define (setup)
    'DoNothing)

  (define (go)
    'DoNothing)

  (run-world setup go)
]

At this point, a blank world should appear.

This is not very exciting.

@subsection{Setting up the turtles}

I want my turtles scattered around the world randomly, and I'd like them to be pointing in random directions as well.

@racketblock[
  (define (setup)
    (ask turtles
        (set xcor (random (get global world-cols)))
        (set ycor (random (get global world-rows)))
        (set direction (random 360))
    ))
]

The first thing we see here is the ask form. This is how we can ask (for example) all of the turtles in the world to do something.

@racketblock[
  (ask turtles
    ...
  )
]

As part of setting up the world, I ask all my turtles to do three things. All three involve setting some of their internal properties. For example, all tutles know their x-coordinate in the world (abbreviated @emph{xcor}), their y-coordinate (@emph{ycor}), and their direction. I can, inside of an ask, request that a turtle set those properties of themselves.

@racketblock[
  (ask turtles
    (set xcor (random (get global world-cols)))
    ...
  )
]

The set form takes the name of the property that I want each individual turtle to change (eg. @emph{xcor}), and then the value I want them to set it to. In this case, I first @emph{get} the number of columns in the world (which is a property of the world, so it is @emph{global}), and then I use the function random to generate a number between zero and one-less-than the number of columns in the world. In short, this means that I am asking each turtle to set its x-coordinate in the world to something between 0 and 99, inclusive.

I repeat this with the y-coordinate. 

@racketblock[
  (ask turtles
    (set xcor (random (get global world-cols)))
    (set ycor (random (get global world-rows)))
    ...
  )
]

And, finally, I ask the turtle to point itself in a random direction, from 0 degrees (straight up) to 359 degrees (nearly straight up, but pointed slightly to the left). 

@racketblock[
  (define (setup)
    (ask turtles
        (set xcor (random (get global world-cols)))
        (set ycor (random (get global world-rows)))
        (set direction (random 360))
    ))
]

Now, if I run the world again, I should see turtles happily scattered all over the world!

@subsection{Moving the turtles}

Finally, I need to do something in the go function. All turtles know how to move.

@racketblock[
  (define (go)
    (ask turtles
      (move 1))
    )
]

Almost everything we do with turtles in the world will be under some sort of ask. Here, I'm asking each turtle, in turn, to move forward one patch. 

If you run the world, you'll see your turtles start to zoom around!

@subsection{Slowing things down}

Finally, as I bring it all together, I might want to add a small pause between ticks of the clock. This adds a delay between one run of the go function and the next.

@racketblock[
(tick-pause (/ 1 30))
]

I typically would put this somewhere up near the creation of the world.

@subsection{All together now}

Your final paddle program should look like this:


@racketblock[
#lang racket

(require paddle)
(make-world 100)

(create-breed turtle turtles)
(create turtles 100)

(tick-pause (/ 1 30))

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

  (run-world setup go)
]

@section{How to ask nicely}

FIXME Write more about ask, and agentsets.

@section{What turtles have}

FIXME Talk about agent parameters, and the (give ...) form.

@section{Manipulating patches}

FIXME How to interact with the world through patches.

@section{Building an interface}

FIXME How to specify an interface, and use the variables it introduces.

@section{Studying the world through plots}

FIXME How to use the plotting interface to study (say) population dynamics.

FIXME Implement the plotting interface.

@section{Logging data}

FIXME Logging data for use in other programs.

FIXME Implement a logging interface.
