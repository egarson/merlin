# Merlin

Merlin is a mocking framework for Erlang to support Test-Driven _Design_; it differs from Test-Driven *Development*, which is the purview of e.g. eunit.

Merlin is a simple library that you include in your unit tests (whether eunit or ct).

## Mocking is Not Just for Object-Oriented Languages

Mocking is an advanced code development technique that enables you to specify the behavior of an unimplemented module - the mock - to drive the collaboration design of the module under test. Mocking is *complementary* to unit testing, and tests behaviour as opposed to focusing on the 'integrity' of the module, as is the emphasis with classical unit testing.

If you aren't familiar with mocking, the first most important thing to understand is that [Mocks Aren't Stubs](http://martinfowler.com/articles/mocksArentStubs.html).

Mocking was introduced introduced at OOPSLA 2004 along with the paper [Mock Roles Not Objects](http://www.jmock.org/oopsla2004.pdf). That paper is a 'must read' if you are new to mocking.

## What About Meck?

[Meck](https://github.com/eproxus/meck) is another mocking framework for Erlang. Meck is is a full-blown OTP application with EasyMock-style syntax, while Merlin is a lightweight library with a JMock-style syntax.

Meck has a large community, healthy adoption and has more capabilities than Merlin: it is probably what you should use for your mocking needs. I only put this out there for fun, and because I've been hacking on it intermittently since 2009. Had I based the syntax on EasyMock, I would not have open-sourced it in light of Meck. However there are enough differences in the simple case that I thought to share it for interest's sake.

## State of Affairs

This project is in an *early beta* phase (despite its age) and is largely experimental. Although the existing programmatic interface will likely remain stable, I do not guarantee it at this juncture: use it at your refactoring peril!

## Building Merlin

Merlin currently has a single dependency to [parse_num](https://github.com/egarson/parse_num), a small library I wrote to convert atoms with a 'numeric' representation into scalars.

Running `make build` will pull down the required libraries and compile all the code.

You should run `make test` to check that everything went as expected.

## License

Copyright (c) 2012 Edward Garson

Distributed under the terms of the MIT license as described by the LICENSE file distributed with this source code.
