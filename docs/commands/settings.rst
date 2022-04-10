.. _settings:

Settings
==============================

The `settings` command allows the user to configure the bot on a per-server or per-channel basis. Each subcommand controls a specific setting.

.. _bot_mod_roles:

``bot_mod_roles``
------------------------

``/settings bot_mod_roles`` gets or sets the roles that are allowed to change the bot's settings on the server; this is the only setting that cannot be altered on a per-channel basis.

Options:
    ``role``:
      Optional. If provided, the role to add or remove to the list of bot mods. If not provided, the command instead returns a list of all bot mod roles.

.. _charsign:
.. _charsep:

``charsign``
``charsep``
------------------------

``/settings charsign`` and ``/settings charsep`` get or set their respective symbols in the channel or server. See Dice Reference for how to use charsign and charsep to embed character data in a roll.

Options:
    ``target``:
      Required. Either ``channel`` or ``server``. If ``channel``, the symbol will be checked or set in the channel the command was issued in; if ``server``, the server.

    ``charsign``/ ``charsep``:
      Optional. If given, set the target symbol's value; otherwise, simply check it.

.. _codex:

``codex``
------------------------

``/settings codex`` gets or sets the server or channel's codex. After typing the command, a list of available codexes will be provided, which can be selected via the a select menu.

Options:
    ``target``:
      Required. Either ``channel`` or ``server``. If ``channel``, the codex change will be targeted to the channel the command was issued in; if ``server``, the server.

.. _ephemeral:

``ephemeral``
------------------------

``/settings ephemeral`` toggles whether command messages will be 'ephemeral' or not. Ephemeral messages will only be visible to the user who sent the command.

Options:
    ``target``:
      Required. Either ``channel`` or ``server``. If ``channel``, the ephemeral setting will be targeted to the channel the command was issued in; if ``server``, the server.

.. _freeze:

``freeze_on_close``
------------------------

``/settings freeze_on_close`` toggles whether paginator results will be frozen when they time out, or just deleted.

Options:
    ``target``:
      Required. Either ``channel`` or ``server``. If ``channel``, the frozen setting will be targeted to the channel the command was issued in; if ``server``, the server.
