
If we were to run the code  this point, a blank world should appear.

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

@codeblock|{

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
}|


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

---

TODO Figure out how to do multi-page scribbles.

TODO Decide whether a first or second person voice makes sense for documentation.

TODO Embed a video.

TODO Notion of why-docs and how-docs is important, and I think these should just be how-docs, and the why-docs are a separate document written in Pollen as their own site.

