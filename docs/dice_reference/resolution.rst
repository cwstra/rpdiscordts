.. _resolution:

Resolution
==============================

Since we're talking about a *dice* parser, one of the most important aspects is how we choose when to *resolve* dice, i.e. when we choose to actually roll the pool.

This can happen in three situations:
1. A function that requires a non-die argument receives a dice pool instead.
2. The user has wrapped a value in brackets (`[]`), in which case the contents of the brackets will be simplified, then resolved.
2. The expression has been fully simplified, and so all that's left to do is roll the remaining dice pools.

This can be important with some operators, such as `<`:

::

   2d6<3

tells the parser to roll `2d6`, keeping the values less than `3`, and adding the results together. (If no results were less than 3, then the total would be zero.)

::

   [2d6]<3

tells the parser to roll `2d6`, *and then* check if the total is less than 3, returning `True` or `False`.
