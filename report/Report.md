# Skeletal semantics for actors, channels, and their translations


# Introduction

This project focuses on the development of skeletal semantics for two
programming languages that employ two message-[assing concurrency models: the
actor model and the channles model.

Two semantics have been developed for each langauge. One that interprets and
computes the result of a given structure in the langauge, and one that
translates the language into the other model.

Together, the result is four software tools.

# Implementation

As explained before, the project is divided into 4 components. The actors
interpreter, the channels interpreter, the actors-to-channels translator and the
channels-to-actors translator.

Each tool is built using Skel and OCaml. Skel is a domain specific langauge that
is used to describe skeletal semantics in a programmatic style. From Skel, a
machine-generated base implementation of the semantics can be created, which
needs to be supplemented with further definitions and types for all behaviour
that cannot be directly captured by the semantics or which is better implemented
with native code (such as binary-encoded integer arithmetic).

Much of the code among the various tools is duplicated--both langauges use the
lambda calculus as a base, share many of the same features and implementations,
and the translation tools emply type definitions directly off of both languages.
The architecture of the machine-generated interpreter implementation is not well
suited for reuse of elements. This is not strictly speaking a problem, but it
does pose an issue in terms of code organization, scalability, bug-fixing, and
other practical concerns when maintaining and updating software made with the
help of necro.

Some minor utilities arte shared among the tools but, due to the architecture of
the generated code, most code is isolated and not easily reusable.
