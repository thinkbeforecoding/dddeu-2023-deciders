# Aggregate composition: a new view on Aggregates

This code has moved to Codeberg

This is the code of the talk presented at [DDDEu 2023](https://2023.dddeurope.com/) in the EventSourcingLive track.

It starts with the [definition of a decider](./deciders.fsx#L5).

Then it defines two simple deciders, [a bulb](./deciders.fsx#L16) and [a cat](./deciders.fsx#LL72C4-L72C4).

[Some tests](./run.fsx#L25) check the behavior of both deciders.

The rest of the [run.fsx](run.fsx#L102) file uses the deciders using functions from infra.fsx to run deciders [in memory](infra.fsx#L14), [persisting state](infra.fsx#L62) or [persisting events](infra.fsx#L74).


Deciders are composed using the [combine function](run.fsx#L141), the [many function](./run.fsx#L244) and composed with [a process](run.fsx#L284).

Finally, deciders are composed in a custom way using [applicative functor](run.fsx#L308).

