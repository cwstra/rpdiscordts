.. _reference:

Reference
========================

Reference commands are used to look up data from the current server or channel's codex. See the :ref:```settings_codex`` command for how to change that.

.. _ref:

``ref``
------------------------

``/ref`` looks through the codex to find a matching entry to the provided data.

Options:
    ``entry``:
      Required. The entry title to look for.

    ``selected_fields``:
      Optional. Space-separated list of fields to include in the result.

.. _top:

``top``
------------------------

``/top`` looks through the codex to find the top matching entries to the provided data.

Options:
    ``entry``:
      Required. The entry title to look for.

    ``count``:
      Optional. The number of top entries to return. If not provided, will default to 5.
