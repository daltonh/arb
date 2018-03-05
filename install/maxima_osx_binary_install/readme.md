The files in this folder are an aid to using a binary version of maxima in place of the macports version on osx.

1. First download the binary maxima version using this link:

https://sourceforge.net/projects/maxima/files/Maxima-MacOS/5.38.0-MacOSX/

This version 5.38 seems to be the most compatible.

2. Copy Maxima.app to your /Applications directory (you may also like to copy wxMaxima.app, and get that working at the same time).

3. Finally copy the file maxima from this directory to somewhere that is in your path, for example:

```bash
cp maxima ~/bin
```

4. Test that everything is working by typing

```bash
maxima
```

To exit maxima type quit();
