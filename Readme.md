[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Jappiejappie](https://img.shields.io/badge/twitch.tv-jappiejappie-purple?logo=twitch)](https://www.twitch.tv/jappiejappie)
[![Githbu actions build status](https://img.shields.io/github/workflow/status/jappeace/ctrlc/Test)](https://github.com/jappeace/ctrlc/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)

**Warning** This is a WIP, the test show that this doesn't quite work as expected.
I confirmed a deadlock situation.
+ https://www.fpcomplete.com/blog/2016/06/async-exceptions-stm-deadlocks/
+ https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

Manages thread cleanup for exiting a program.
Tries to codify the ideas from: https://ro-che.info/articles/2014-07-30-bracket#bracket-in-non-main-threads


I think some tests start failing because with printlogger they all start waiting on the buffer to flush
so they fail togehter, whereas other succeed together.

with timeout it all fails?! 

I rewrote it to use the stm and async libs directly.
the queue is a pretty crummy idea but it simplifies things.
async allows me to use their waiting logic.

