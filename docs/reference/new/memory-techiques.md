---
title: Strategies for Working with Memory
---

## TODO

+ memcpy
+ Use stack for small amounts of data.
+ use zone-array stacks for large amounts of data that won't leave the function.
+ swap zones for data you will be returning (e.g. allocate the data for the return object in the containing zone, create all your throw away data in the new zone, and then destroy the zone when you leave) - should probably create macro for this as it's a useful trick. Or even rewrite letz so it does that (if it doesn't already).
+ Use heap for data that will be regularly created and destroyed, and add a hook into the current zone so that it is still managed in the zone (and has zone lifetime), but you can destroy the data as necessary.
+ Show how you can attach zones to user data objects, and manage them that way.
+ Apologize for the lack of const... :)
+ Use the supplied algorithms/data structures in prelude, and pay attention to their strengths and weaknesses.
+ Show the real time memory management thing that SuperCollider Ross Becina do, but show how this can be elegantly done in zones (in fact that's going to be a function/data structure somewhere as it's too useful).