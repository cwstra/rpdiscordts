.. _character:

Character
====================================

This command group allows users to create, edit, and view characters for the current server.

.. _character_new:

``new``
------------------

``/character new`` creates a new character

Options:
    ``name``:
      Required.

    ``attributes``:
      Optional. A list of `Name=Value` pairs. If Name or Value contains spaces, quotation marks are necessary.

      Examples:
        ``Strength=3d6``
        ``Flavor=Blueberry "Star Rating"=5``

.. _character_view:

``view``
------------------

``/character view`` views an existing character.
- If `attributes` are not provided, they will be listed without their values
- If `attributes` are provided, the attributes in the list will be printed out along with their values, assuming they exist on the character.

Options:
    ``name``:
      Required.

    ``attributes``:
      Optional. A space-separated list of attribute names. If a name contains spaces, quotation marks are necessary.

      Examples:
        ``Strength``
        ``Flavor "Star Rating"``

.. _character_rename:

``rename``
------------------

``/character rename`` renames an existing character

Options:
    ``old_name``:
      Required.

    ``new_name``:
      Required.

.. _character_edit:

``edit``
------------------

``/character edit`` modifies an existing character

Options:
    ``name``:
      Required.

    ``attributes``:
      Optional. A list of `Name=Value` pairs or lone `Name` singletons. `Name=Value` pairs will set the character's `Name` attribute to `Value`; `Name` singletons will delete that attribute from the character. If any `Name` or `Value` has spaces, use quotation marks.

      Examples:
        Add a new attribute, or change the value of an existing attribute:
        ``Strength=5d6``
        Remove an existing attribute:
        ``Flavor``
        Perform multiple operations at once.
        ``Wisdom=3d6 Charisma Flavor="Chocolate Chip"``

.. _character_delete:

``delete``
------------------

``/character delete`` deletes an existing character

Options:
    ``name``:
      Required.
