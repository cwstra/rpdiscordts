.. _characters:

Using Characters in Dice
==============================

Information stored in characters can be used in roll commands, using charsigns and charseps.

First, to successfully use a character attribute in a dice roll, it should already be a properly formatted dice string, as the attribute value will be inserted into the string to create the rolled dice pool.

Each server or channel will have charsigns and charseps. By default, the charsign is `$`, and the charsep is `:`. To use a character attribute in a roll, join together the character's name and the attribute name with the charsep, and surround the whole thing with the charsign.

For example, if `Jim the Test Muffin` has a `Wisdom` attribute with a value of `5d6`, we can roll that dice pool by typing

::

   $Jim the Test Muffin:Wisdom$

as our roll query.

This is equivalent to

::

   5d6

This can be augmented with other information; for example:

::

   $Jim the Test Muffin:Wisdom$+5

is equivalent to

::

   5d6+5
