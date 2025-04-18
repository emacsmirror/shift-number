
##################
Emacs Shift Number
##################

About
=====

This Emacs package provides commands to increase and decrease the number
at point (or the next number on the current line).


Installation
============


Automatic
---------

This package can be installed from `MELPA <http://melpa.org/>`__
(with ``M-x package-install`` or ``M-x list-packages``).


Manual
------

For the manual installation, clone the repository, add the directory to
``load-path`` and add autoloads for the interactive commands:

.. code-block:: elisp

   (add-to-list 'load-path "/path/to/shift-number-dir")
   (autoload 'shift-number-up "shift-number" nil t)
   (autoload 'shift-number-down "shift-number" nil t)


Usage
=====

As you can see in the gif demonstration:

- ``M-x shift-number-up`` increases the current number.

  If there is no number at point, the first number between the current position and the end of line is increased.
  With a numeric prefix ARG, the number is increased for this ARG.

- ``M-x shift-number-down`` decreases the current number.

You may bind some keys to these commands in a usual manner, for example:

.. code-block:: elisp

   (global-set-key (kbd "M-+") 'shift-number-up)
   (global-set-key (kbd "M-_") 'shift-number-down)


Custom Variables
----------------

``shift-number-motion``: ``nil``
   When non-nil, move the cursor to the end of the number.
``shift-number-negative``: ``t``
   When non-nil, support negative numbers.


Similar packages
================

There are other packages for the same task (modifying the number at
point):

- `operate-on-number <https://github.com/knu/operate-on-number.el>`__
- `number <https://github.com/chrisdone/number>`__
- `evil-numbers <https://github.com/cofi/evil-numbers>`__

Comparing with them, ``shift-number`` has the following distinctions:

- If there is no number at point, it operates on the next number on the
  current line.

- The point does not move anywhere when a number is modified.

- If a number has leading zeros (for example ``007``), they are preserved
  during shifting.

- It is simple: only shifting up/down is available, no multiplication or
  other more complex stuff.

- It does not prompt for any additional input: you just press a key
  bound to ``shift-number-{up/down}`` command and the number is changing.
