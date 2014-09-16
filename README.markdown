# haskell-sscgi

Why another SCGI library? The maintainer of the [scgi library](http://hackage.haskell.org/package/scgi) was not accepting packages (this was before it was marked as deprecated). For a while I maintained a fork, but eventually decided to get rid of some of the complexity and design decisions introduced by `Network.CGI`.

Currently the library is used by [Vocabulink](https://github.com/jekor/vocabulink) but I plan to get rid of it in favor of communicating HTTP directly (probably with [http-kit](https://github.com/zalora/http-kit)).
