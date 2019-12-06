# SSH keys

> Secure SSH configuration

## Introduction

This started off from [github-keygen](https://github.com/dolmen/github-keygen),
which is also checked in at `home\.bin\github-keygen`.

## Cheat

To generate new keys

```
ssh-keygen -t rsa -b 4096 -C mako/andrew@cates.io -f id_andrew@cates.io
```

Where

- `t` is the key type with a default of `rsa`
- `b` is the key bits with a default of `2048`, prefer 4096
- `C` is the comment which is comprised of `$hostname/$user@domain`.
- `f` is the file name which is comprised of `id_$user@domain`

## Description

- The `config` has a high level of security.
- Create a new SSH **private key dedicated** for each hostname listed in
  `config`. This is much better than using the same SSH key to connect to
  multiple hosts. (If you loose that key, just revoke it.
- Use a different SSH key for each computer you use. This will help you to use
  the best practices in SSH security.
- The [Github manual](https://help.github.com/articles/generating-ssh-keys)
  tells to _remove_ your existing SSH keys. This `config` avoids that; keep your
  keys and your existing SSH config.
- It setups a **very secure SSH configuration**, independent of your other SSH
  settings:
  - Enable only the authentication method used with Github (publickey)
  - Use only the private key dedicated to Github (the IdentitiesOnly of SSH
    config)
  - Setup a dedicated _known_hosts_ file with the Github SSH hosts and enable
    strict host checking (this means that if you get SSH alerts about host key
    problem when connecting to GitHub, this is really a serious error and you
    should check that someone is not altering your network link).
  - Use stronger encryption algorithms than your default SSH setup (following
    [@stribika advices](https://stribika.github.io/2015/01/04/secure-secure-shell.html)); -
    Disable bad things that could come from the Github hosts ("Trust no-one")
- It enables SSH connection sharing (see the **ControlMaster** option in
  [ssh_config(5)](http://search.cpan.org/perldoc?ssh_config\(5\)) and
  [this blog post](http://interrobeng.com/2013/08/25/speed-up-git-5x-to-50x/))
- It creates unique host aliases for github.com/gist.github.com that you'll be
  able to use in Git URLs (`git remote`) to connect to a particular account.
  This gives the flexibility to use **multiple Github accounts** (and therefore
  a different SSH key for each).

```ssh
<account>.github.com:<repo-owner>/<repo>.git  (for each account)
github.com:<repo-owner>/<repo>.git            (for the default account)
```

in addition to:

```ssh
git@github.com:<repo-owner>/<repo>.git
```

## Trust

As with any software that deals with the security of your computer or of
communications with other computers (operating system, antivirus, HTTPS
implementation, password storage...), you have to be able to trust it. (If you
haven't ever asked yourself that question about the software you already use,
you should!)

## Transferring

- [How to transfer pgp private&public key to another computer?](https://stackoverflow.com/questions/3174537/how-to-transfer-pgp-privatepublic-key-to-another-computer/3176373#3176373)
