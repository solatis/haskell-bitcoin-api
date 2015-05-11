haskell-bitcoin-api
===================

[![Build Status](https://travis-ci.org/solatis/haskell-bitcoin-api.png?branch=master)](https://travis-ci.org/solatis/haskell-bitcoin-api)
[![Coverage Status](https://coveralls.io/repos/solatis/haskell-bitcoin-api/badge.svg?branch=master)](https://coveralls.io/r/solatis/haskell-bitcoin-api?branch=master)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

The Bitcoin Core application provides an HTTP RPC interface for communication.
This library implements access to these functions. It builds on top of the
`bitcoin-tx` and `bitcoin-script`, and as such provides an extremely flexible
environment to create, manipulate and store transactions and custom scripts.
