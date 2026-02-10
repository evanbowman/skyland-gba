## Skyland GBA scripts

### Preamble

NOTE: For a long time, the scripts were built around chains of nested callbacks. I later introduced an `await` keyword, allowing functions to be suspended and resumed, along with a promise datatype. I am cleaning up the scripts to make them more legible with the new `await` syntax, but it's a slow process.

If you see a function with the * suffix, usually these functions will return a promise that you can use with `await`. Await is not allowed, unfortunately, within code called indirectly by compiled functions. I.e. use foreach-async rather than the builtin foreach when iterating over lists and calling await in a lambda passed to foreach.


### Where do I start?

A good place to start looking, if you want to play around with some scripts, are the newgame.lisp script, which sets up your island when starting a new adventure, and the event scripts in event/neutral/. The events are organized by zone, followed by event number.
