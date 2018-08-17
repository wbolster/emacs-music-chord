music-chord.el
==============

This is an Emacs major mode for text files containing musical chords.

It provides basic syntax highlighting for the (very non-strict)
notation used on lyrics/chords/tabs web sites to annotate songs with
their structure and chord progressions.

Usage
-----

Open a text file, then execute:

``` emacs
M-x music-chord-mode
```

Example
-------

``` text
[verse]
C7     (3X)
lalala
G7
lala

[chorus x2]
```

Credits
-------

Written by wouter bolsterlee (wbolster).
