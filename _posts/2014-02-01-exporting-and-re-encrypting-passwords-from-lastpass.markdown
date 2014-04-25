---
layout: post
title: "Exporting and Re-Encrypting Passwords from LastPass"
comments: true
categories: automation technical-debt
---

As part of my ongoing process to address my
[personal technical debt](/entries/your-workstation-is-a-massive-technical-debt/),
I have been trying to figure out how to handle my passwords.
See, I have not maintained good personal password policies. To help get
control of that, I've been using
[LastPass](https://lastpass.com/).

Unfortunately, LastPass is not
[free software](http://www.gnu.org/philosophy/free-sw.html).
I use closed source software like most other practically-minded
people, but I think of it as
a risky choice, and as its
[own form of technical debt](/entries/closed-source-as-risk).
There is something troubling about replacing one form of technical
debt with another that I don't like.

That being said, I am trying to be more practical and less
ideological, whenever possible.
Good enough is good enough. Better is better. Perfection doesn't exist.
Thus, I think LastPass is a reasonably good way to tackle
my password problems.


However, one requirement I have is to be able to export my lastpass
passwords. I don't want to be dependent upon LastPass for my entire
online life.

I was able to hack together a nice little script to do this for me.
Basically, it:

1. Prompts the user for LastPass authentication data. Passwords are
   read via `IO#noecho` so your passwords won't be visible on the
   console.

1. Contacts LastPass and downloads the password database. The
   [LastPass ruby gem](https://github.com/detunized/lastpass-ruby)
   makes this easy.

2. Prompts the user for a password to encrypt the downloaded lastpass
   data with.

3. Uses the gpg command to create a password-encrypted database.



The code is available
[on Github](https://github.com/joelmccracken/dotfiles/blob/master/bin/lastpass-backup).
It requires the lastpass gem to be installed, along with gpg.


Example Usage:

    bash-3.2$ lastpass-backup ~/Dropbox/foo.gpg
    Lastpass Email:
    Lastpass Password:
    Connecting to lastpass
    GPG Passphrase to encrypt export:
    You access the export by running:
        gpg -d /Users/joel/Dropbox/foo.gpg
    And entering the GPG passphrase you used.

Accessing the database:

    bash-3.2$ gpg -d ~/Dropbox/foo.gpg
    gpg: CAST5 encrypted data
    gpg: encrypted with 1 passphrase
    ...


