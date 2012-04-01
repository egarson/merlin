# Merlin

Merlin is a mocking framework for Erlang to support Test-Driven *Design*, as opposed to *Development*, which is the purview of e.g. eunit.

## Mocking is Not Just for Object-Oriented Languages

If you aren't familiar with mocking, the first most important thing to understand is that [Mocks Aren't Stubs](http://martinfowler.com/articles/mocksArentStubs.html).

Mocking is an advanced code development technique that enables you to specify the behavior of an unimplemented module - the mock - to drive the collaboration design of the module under test. Mocking is *complementary to unit testing* and tests behaviour as opposed to focusing purely on the 'integrity' of the module, as is the case with classical unit testing.

Mocking was introduced in the paper [Mock Roles Not Objects](http://www.jmock.org/oopsla2004.pdf).

## State of Affairs

This project is currently in *early beta* and should be regarded as completely experimental. That said, the programmatic interface is relatively stable, while the implementation is still quite volatile.

## Dependencies

* parse_num

Github does not support 'git submodules' at the time of this writing. As a consequence, you will need to put a copy of [parse_num.erl](https://github.com/egarson/parse_num) in the `src/` directory.

## License

Copyright (c) 2012 Edward Garson

Distributed under the terms of the MIT license as described by the included MIT-LICENSE.txt.
