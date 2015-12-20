# Structure and Interpretation of Classical Mechanics

The goal of this repository is to provide the solutions for the exercises of the book "Structure and Interpretation of Classical Mechanics", by Gerald Jay Sussman and Jack Wisdom. 

Feel free to contribute!

## How to use

### Scheme and Scmutils lib

Reference: http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm

This is mainly for linux.

1) Download the tarball from http://groups.csail.mit.edu/mac/users/gjs/6946/scmutils-tarballs/

Ex: For my system (linux 64-bit), I downloaded the file scmutils-20140302-x86-64-gnu-linux.tar.gz

2) Log into root with

``` sudo -s ```

3) Execute:

```
cd /usr/local
tar -xvzf [path-to-your-tarball.tar.gz]
```


4) To start Scheme, use

``` /usr/local/bin/mechanics ```

Or, if this directory is in your path, use

``` mechanics ```

5) Useful documentation is under /usr/local/scmutils/manual/

### Edwin and Emacs

You can use scheme/scmutils with the interface provided by the tarball (Edwin) or with Emacs, if you areadly have it installed and prefer to.

Edwin is an Emacs-like editor, and it opens when you run "mechanics".

To use scheme/scmutils with Emacs, include the following in your .emacs:

```
(defun mechanics ()
  (interactive)
  (run-scheme
    "ROOT/mit-scheme/bin/scheme --library ROOT/mit-scheme/lib"
    ))
```

Replacing ROOT with the directory in which smutils is installed, in my case /usr/local/scmutils.
Now, restart emacs (or use C-x C-e to evaluate the function above), and run the environment with M-x mechanics.
Checkout this reference for more: http://redsymbol.net/articles/using-gnu-emacs-with-scmutils/

### Useful links

Edwin cheat sheet: http://groups.csail.mit.edu/mac/users/gjs/6946/cheat-sheet.pdf

Scmutils begginer's guide: http://groups.csail.mit.edu/mac/users/gjs/6946/beginner.pdf

Scmutils manual: http://groups.csail.mit.edu/mac/users/gjs/6946/refman.pdf
