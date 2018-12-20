;;; Copyright 2018 Matt Jadud <matt@jadud.com>
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
;;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#lang racket

(provide (all-defined-out))


;; For this to work, I need individual agents.
;; They can all be the same. They'll keep their fields
;; in a hash. I debated calling that field "has", so it would be
;; (agent-has ...) as the accessor...
(struct agent (id breed fields) #:transparent)
;; An agentset should be a struct, so I know when I'm working with it.
(struct agentset (breed plural base-fields agents)
  #:transparent #:mutable)

(struct coordinate (x y) #:transparent)

(struct rgb-color (r g b) #:transparent)

(struct iv (agent-variable value agentset) #:transparent)
(struct slider (agentset var low high)
  #:transparent)

