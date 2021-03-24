# Counting unique words in Haskell

Inspired by Ben Hoyt's post [1], which was pointed out to me by
@CarlHedgren.

What we have:
- implementations using streaming, pipes, and conduit

What we want:
- implementation using streamly
- naive implementation using nothing but base?
- automate (performance) testing between all of the above
- possibly PR a solution to [1]




[1] https://benhoyt.com/writings/count-words





- conduit bs            ~  542ms
- conduit stream        ~  572ms
- megaparsec            ~  879ms
- pipes                 ~ 2090ms
- boarders              ~  305ms
- streaming bs          ~  557ms
- streaming str         ~  745ms
- streamlyt             ~ 1010ms





TODO after stream:
- figure out and try -fllvm
- play with the ByteString version a bit more
- we need to be able to profile better, see more details (maybe add more CC)
- have one more go for the other versions without -threaded


