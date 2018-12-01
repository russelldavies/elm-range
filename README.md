# russelldavies/elm-range

Model and operate on a range of values in Elm.

```elm
> range1 = (Range.Int.fromString "[1,5]")
Ok (Bounded (Inclusive 1,Exclusive 6))
    : Result (List Parser.DeadEnd) (Range.Range Int)

> range2 = (Range.Int.fromString "(4,10]")
Ok (Bounded (Inclusive 5,Exclusive 11))
    : Result (List Parser.DeadEnd) (Range.Range Int)

> Result.map2 Range.Int.merge range1 range2
Ok (Bounded (Inclusive 1,Exclusive 11))
    : Result (List Parser.DeadEnd) (Range.Range Int)
```

## Overview

The concept of a range is inspired from PostgreSQL's [Range
Types](https://www.postgresql.org/docs/current/rangetypes.html):

> Range types are data types representing a range of values of some element
> type (called the range's *subtype*). For instance, ranges of `timestamp`
> might be used to represent the ranges of time that a meeting room is
> reserved. In this case the data type is `tsrange` (short for “timestamp
> range”), and `timestamp` is the subtype. The subtype must have a total order
> so that it is well-defined whether element values are within, before, or
> after a range of values.
>
> Range types are useful because they represent many element values in a single
> range value, and because concepts such as overlapping ranges can be expressed
> clearly. The use of time and date ranges for scheduling purposes is the
> clearest example; but price ranges, measurement ranges from an instrument,
> and so forth can also be useful.
