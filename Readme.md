[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/ctrlc/Test)](https://github.com/jappeace/ctrlc/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

Manages thread cleanup for exiting a program.
Tries to codify the ideas from:
https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads

# todos

+ Maybe it's possible to track all threads without having to
  pass around a structure.
+ see if this works for cabal download phase?
+ see if this works for warp (and confirm issues from blog)

# References
I'm sorry, all of this is important, there are so many obnoxious details
involved.

+ https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf
+ https://www.fpcomplete.com/blog/2016/06/async-exceptions-stm-deadlocks/
+ https://ro-che.info/articles/2014-07-30-bracket
+ https://www.fpcomplete.com/blog/2018/04/async-exception-handling-haskell/
+ https://simonmar.github.io/posts/2017-01-24-asynchronous-exceptions.html
+ https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Exception.html#v:throwTo
+ https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Conc-Sync.html#t:BlockReason
