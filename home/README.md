# Yosemite Upgrade Changes Open File Limit

OSX has a ridiculously low limit on the maximum number of open files. If you use OSX to develop [Node][1] applications -- or even if you just use Node tools like [grunt][2] or [gulp][3] \-- you've no doubt run into this issue.

To address this, I have this line in my `$HOME/.bash_profile`:

    ulimit -n 1000000 unlimited

And a corresponding entry in `/etc/launchd.conf`:

    limit maxfiles 1000000

That solved the problem until I upgraded to OSX Yosemite, after which I began seeing the following error every time I opened a terminal window:

    bash: ulimit: open files: cannot modify limit: Invalid argument

Luckily, I a little Google foo yielded [this Superuser post (and answer)][4].

So it was a quick fix:

    $ echo kern.maxfiles=65536 | sudo tee -a /etc/sysctl.conf
    $ echo kern.maxfilesperproc=65536 | sudo tee -a /etc/sysctl.conf
    $ sudo sysctl -w kern.maxfiles=65536
    $ sudo sysctl -w kern.maxfilesperproc=65536
    $ ulimit -n 65536 65536

Then I updated my `$HOME/.bash_profile` to change the `ulimit` directive to match that last command, above, and I was back in business.

##  Terminal

To enable true color use:

```
tic -x -o ~/.terminfo xterm-24bit.terminfo
```

And then:

```
export TERM=xterm-24bit
```

Add the following to the tmux config:

```
set -g default-terminal "xterm-24bit"
set -g terminal-overrides ',xterm-24bit:Tc'
```

And add:

```
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
```

## GPG

Let's setup GPG for commit signing and other shenanigans.

### Keybase

Keybase is a great way to start using GPG, they'll generate your keys and host them for you.

If you don't yet have an account, go to [https://keybase.io](https://keybase.io) and create it.

Then login on your computer and the `keybase id` command should output your data.

```bash
keybase login
keybase id
```

#### Making Keybase behave

Keybase by default installs a lot of crap you probably don't need. A slow JS-based UI, Fuse encrypted networking, chats, etc.

The following will trim all the fat and leave the good: CLI.

```bash
/Applications/Keybase.app/Contents/SharedSupport/bin/keybase install -c cli
keybase ctl stop
keybase uninstall -c fuse
keybase uninstall -c helper
keybase uninstall -c kbfs
keybase uninstall -c service
keybase uninstall -c updater
sudo pkill -TERM keybase.Helper
sudo rm -f /Library/LaunchDaemons/keybase.Helper.plist
sudo rm -f /Library/PrivilegedHelperTools/keybase.Helper
```

An essential step is also aliasing the `keybase` command to `keybase --standalone`, which will stop complaining about the agent not running in the background.

This is accomplished in the `aliases.fish` file, so you don't need to do it.

As a final step, let's make Keybase use the `pinentry-mac` so our passwords can be securely stored in the OS X Keychain.

```bash
keybase config set pinentry.path /usr/local/bin/pinentry-mac
```

#### If you don't have a GPG key yet

Let's create a new key on Keybase specifically for use in GPG on our computer.

```bash
keybase pgp gen --multi
```

#### If you have a GPG key already

If you have your key on another machine, you can run `keybase pgp export` to see all the available keys to you.
Otherwise skip to the [Importing existing GPG key].

```bash
$ keybase pgp export
# ▶ WARNING Found several matches:
# user: Miha Rebernik <miha@rebernik.info>
# 4096-bit RSA key, ID 6B997648324AF29E, created 2019-08-19
```

Now export it with (it'll ask you for a password with which to encrypt it, you should add it):

```bash
keybase pgp export -q 6B997648324AF29E -s > pgp_key
```

Now get your `pgp_key` file to the new machine where you're setting GPG up.

##### Importing existing GPG key

Run this command to import the private key into Keybase first, and then into GPG.

This will first ask you for the password you used to encrypt this key, so it can decrypt it and import it.
Then it will also ask you for a password with which to securely store the key on your machine. This password
will be needed every time you try to use this key to sign or decrypt something (it will later be handled transparently by `pinentry-mac`).

```bash
keybase pgp import -i pgp_key
keybase pgp export -q 6B997648324AF29E --secret | gpg --allow-secret-key-import --import
```

### Setting GPG and Git

Now we'll list available keys and set the default key for Git

```bash
$ gpg --list-secret-keys --keyid-format LONG
# /Users/mihar/.gnupg/pubring.kbx
# -------------------------------
# sec   rsa4096/6B997648324AF29E 2019-08-19 [SC] [expires: 2035-08-15]
# uid                 [ unknown] Miha Rebernik <miha@rebernik.info>
# ssb   rsa4096/BB08A4DB17E7EC97 2019-08-19 [E] [expires: 2035-08-15]

$ git config --global user.signingkey 6B997648324AF29E
$ git config --global commit.gpgsign true
```

If you need to add the key to Github or anywhere else, you can use this command:

```bash
keybase pgp export -q 6B997648324AF29E | pbcopy
```

Now link the config files in this repo:

```bash
ln -s ~/Code/dotenv/home/gpg.conf ~/.gnupg/gpg.conf
ln -s ~/Code/dotenv/home/gpg-agent.conf ~/.gnupg/gpg-agent.conf
```

And restart the GPG agent:

```bash
gpgconf --kill gpg-agent
```

### Testing GPG

Test everything works as needed by doing:

```bash
echo "testing GPG" | gpg --clearsign
```

You should see no errors and a plaintest message you've inputted along with a signature.## GPG

Let's setup GPG for commit signing and other shenanigans.

### Keybase

Keybase is a great way to start using GPG, they'll generate your keys and host them for you.

If you don't yet have an account, go to [https://keybase.io](https://keybase.io) and create it.

Then login on your computer and the `keybase id` command should output your data.

```bash
keybase login
keybase id
```

#### Making Keybase behave

Keybase by default installs a lot of crap you probably don't need. A slow JS-based UI, Fuse encrypted networking, chats, etc.

The following will trim all the fat and leave the good: CLI.

```bash
/Applications/Keybase.app/Contents/SharedSupport/bin/keybase install -c cli
keybase ctl stop
keybase uninstall -c fuse
keybase uninstall -c helper
keybase uninstall -c kbfs
keybase uninstall -c service
keybase uninstall -c updater
sudo pkill -TERM keybase.Helper
sudo rm -f /Library/LaunchDaemons/keybase.Helper.plist
sudo rm -f /Library/PrivilegedHelperTools/keybase.Helper
```

An essential step is also aliasing the `keybase` command to `keybase --standalone`, which will stop complaining about the agent not running in the background.

This is accomplished in the `aliases.fish` file, so you don't need to do it.

As a final step, let's make Keybase use the `pinentry-mac` so our passwords can be securely stored in the OS X Keychain.

```bash
keybase config set pinentry.path /usr/local/bin/pinentry-mac
```

#### If you don't have a GPG key yet

Let's create a new key on Keybase specifically for use in GPG on our computer.

```bash
keybase pgp gen --multi
```

#### If you have a GPG key already

If you have your key on another machine, you can run `keybase pgp export` to see all the available keys to you.
Otherwise skip to the [Importing existing GPG key].

```bash
$ keybase pgp export
# ▶ WARNING Found several matches:
# user: Miha Rebernik <miha@rebernik.info>
# 4096-bit RSA key, ID 6B997648324AF29E, created 2019-08-19
```

Now export it with (it'll ask you for a password with which to encrypt it, you should add it):

```bash
keybase pgp export -q 6B997648324AF29E -s > pgp_key
```

Now get your `pgp_key` file to the new machine where you're setting GPG up.

##### Importing existing GPG key

Run this command to import the private key into Keybase first, and then into GPG.

This will first ask you for the password you used to encrypt this key, so it can decrypt it and import it.
Then it will also ask you for a password with which to securely store the key on your machine. This password
will be needed every time you try to use this key to sign or decrypt something (it will later be handled transparently by `pinentry-mac`).

```bash
keybase pgp import -i pgp_key
keybase pgp export -q 6B997648324AF29E --secret | gpg --allow-secret-key-import --import
```

### Setting GPG and Git

Now we'll list available keys and set the default key for Git

```bash
$ gpg --list-secret-keys --keyid-format LONG
# /Users/mihar/.gnupg/pubring.kbx
# -------------------------------
# sec   rsa4096/6B997648324AF29E 2019-08-19 [SC] [expires: 2035-08-15]
# uid                 [ unknown] Miha Rebernik <miha@rebernik.info>
# ssb   rsa4096/BB08A4DB17E7EC97 2019-08-19 [E] [expires: 2035-08-15]

$ git config --global user.signingkey 6B997648324AF29E
$ git config --global commit.gpgsign true
```

If you need to add the key to Github or anywhere else, you can use this command:

```bash
keybase pgp export -q 6B997648324AF29E | pbcopy
```

Now link the config files in this repo:

```bash
ln -s ~/Code/dotenv/home/gpg.conf ~/.gnupg/gpg.conf
ln -s ~/Code/dotenv/home/gpg-agent.conf ~/.gnupg/gpg-agent.conf
```

And restart the GPG agent:

```bash
gpgconf --kill gpg-agent
```

### Testing GPG

Test everything works as needed by doing:

```bash
echo "testing GPG" | gpg --clearsign
```

You should see no errors and a plaintest message you've inputted along with a signature.## GPG

Let's setup GPG for commit signing and other shenanigans.

### Keybase

Keybase is a great way to start using GPG, they'll generate your keys and host them for you.

If you don't yet have an account, go to [https://keybase.io](https://keybase.io) and create it.

Then login on your computer and the `keybase id` command should output your data.

```bash
keybase login
keybase id
```

#### Making Keybase behave

Keybase by default installs a lot of crap you probably don't need. A slow JS-based UI, Fuse encrypted networking, chats, etc.

The following will trim all the fat and leave the good: CLI.

```bash
/Applications/Keybase.app/Contents/SharedSupport/bin/keybase install -c cli
keybase ctl stop
keybase uninstall -c fuse
keybase uninstall -c helper
keybase uninstall -c kbfs
keybase uninstall -c service
keybase uninstall -c updater
sudo pkill -TERM keybase.Helper
sudo rm -f /Library/LaunchDaemons/keybase.Helper.plist
sudo rm -f /Library/PrivilegedHelperTools/keybase.Helper
```

An essential step is also aliasing the `keybase` command to `keybase --standalone`, which will stop complaining about the agent not running in the background.

This is accomplished in the `aliases.fish` file, so you don't need to do it.

As a final step, let's make Keybase use the `pinentry-mac` so our passwords can be securely stored in the OS X Keychain.

```bash
keybase config set pinentry.path /usr/local/bin/pinentry-mac
```

#### If you don't have a GPG key yet

Let's create a new key on Keybase specifically for use in GPG on our computer.

```bash
keybase pgp gen --multi
```

#### If you have a GPG key already

If you have your key on another machine, you can run `keybase pgp export` to see all the available keys to you.
Otherwise skip to the [Importing existing GPG key].

```bash
$ keybase pgp export
# ▶ WARNING Found several matches:
# user: Miha Rebernik <miha@rebernik.info>
# 4096-bit RSA key, ID 6B997648324AF29E, created 2019-08-19
```

Now export it with (it'll ask you for a password with which to encrypt it, you should add it):

```bash
keybase pgp export -q 6B997648324AF29E -s > pgp_key
```

Now get your `pgp_key` file to the new machine where you're setting GPG up.

##### Importing existing GPG key

Run this command to import the private key into Keybase first, and then into GPG.

This will first ask you for the password you used to encrypt this key, so it can decrypt it and import it.
Then it will also ask you for a password with which to securely store the key on your machine. This password
will be needed every time you try to use this key to sign or decrypt something (it will later be handled transparently by `pinentry-mac`).

```bash
keybase pgp import -i pgp_key
keybase pgp export -q 6B997648324AF29E --secret | gpg --allow-secret-key-import --import
```

### Setting GPG and Git

Now we'll list available keys and set the default key for Git

```bash
$ gpg --list-secret-keys --keyid-format LONG
# /Users/mihar/.gnupg/pubring.kbx
# -------------------------------
# sec   rsa4096/6B997648324AF29E 2019-08-19 [SC] [expires: 2035-08-15]
# uid                 [ unknown] Miha Rebernik <miha@rebernik.info>
# ssb   rsa4096/BB08A4DB17E7EC97 2019-08-19 [E] [expires: 2035-08-15]

$ git config --global user.signingkey 6B997648324AF29E
$ git config --global commit.gpgsign true
```

If you need to add the key to Github or anywhere else, you can use this command:

```bash
keybase pgp export -q 6B997648324AF29E | pbcopy
```

Now link the config files in this repo:

```bash
ln -s ~/Code/dotenv/home/gpg.conf ~/.gnupg/gpg.conf
ln -s ~/Code/dotenv/home/gpg-agent.conf ~/.gnupg/gpg-agent.conf
```

And restart the GPG agent:

```bash
gpgconf --kill gpg-agent
```

### Testing GPG

Test everything works as needed by doing:

```bash
echo "testing GPG" | gpg --clearsign
```

You should see no errors and a plaintest message you've inputted along with a signature.## GPG

Let's setup GPG for commit signing and other shenanigans.

### Keybase

Keybase is a great way to start using GPG, they'll generate your keys and host them for you.

If you don't yet have an account, go to [https://keybase.io](https://keybase.io) and create it.

Then login on your computer and the `keybase id` command should output your data.

```bash
keybase login
keybase id
```

#### Making Keybase behave

Keybase by default installs a lot of crap you probably don't need. A slow JS-based UI, Fuse encrypted networking, chats, etc.

The following will trim all the fat and leave the good: CLI.

```bash
/Applications/Keybase.app/Contents/SharedSupport/bin/keybase install -c cli
keybase ctl stop
keybase uninstall -c fuse
keybase uninstall -c helper
keybase uninstall -c kbfs
keybase uninstall -c service
keybase uninstall -c updater
sudo pkill -TERM keybase.Helper
sudo rm -f /Library/LaunchDaemons/keybase.Helper.plist
sudo rm -f /Library/PrivilegedHelperTools/keybase.Helper
```

An essential step is also aliasing the `keybase` command to `keybase --standalone`, which will stop complaining about the agent not running in the background.

This is accomplished in the `aliases.fish` file, so you don't need to do it.

As a final step, let's make Keybase use the `pinentry-mac` so our passwords can be securely stored in the OS X Keychain.

```bash
keybase config set pinentry.path /usr/local/bin/pinentry-mac
```

#### If you don't have a GPG key yet

Let's create a new key on Keybase specifically for use in GPG on our computer.

```bash
keybase pgp gen --multi
```

#### If you have a GPG key already

If you have your key on another machine, you can run `keybase pgp export` to see all the available keys to you.
Otherwise skip to the [Importing existing GPG key].

```bash
$ keybase pgp export
# ▶ WARNING Found several matches:
# user: Miha Rebernik <miha@rebernik.info>
# 4096-bit RSA key, ID 6B997648324AF29E, created 2019-08-19
```

Now export it with (it'll ask you for a password with which to encrypt it, you should add it):

```bash
keybase pgp export -q 6B997648324AF29E -s > pgp_key
```

Now get your `pgp_key` file to the new machine where you're setting GPG up.

##### Importing existing GPG key

Run this command to import the private key into Keybase first, and then into GPG.

This will first ask you for the password you used to encrypt this key, so it can decrypt it and import it.
Then it will also ask you for a password with which to securely store the key on your machine. This password
will be needed every time you try to use this key to sign or decrypt something (it will later be handled transparently by `pinentry-mac`).

```bash
keybase pgp import -i pgp_key
keybase pgp export -q 6B997648324AF29E --secret | gpg --allow-secret-key-import --import
```

### Setting GPG and Git

Now we'll list available keys and set the default key for Git

```bash
$ gpg --list-secret-keys --keyid-format LONG
# /Users/mihar/.gnupg/pubring.kbx
# -------------------------------
# sec   rsa4096/6B997648324AF29E 2019-08-19 [SC] [expires: 2035-08-15]
# uid                 [ unknown] Miha Rebernik <miha@rebernik.info>
# ssb   rsa4096/BB08A4DB17E7EC97 2019-08-19 [E] [expires: 2035-08-15]

$ git config --global user.signingkey 6B997648324AF29E
$ git config --global commit.gpgsign true
```

If you need to add the key to Github or anywhere else, you can use this command:

```bash
keybase pgp export -q 6B997648324AF29E | pbcopy
```

Now link the config files in this repo:

```bash
ln -s ~/Code/dotenv/home/gpg.conf ~/.gnupg/gpg.conf
ln -s ~/Code/dotenv/home/gpg-agent.conf ~/.gnupg/gpg-agent.conf
```

And restart the GPG agent:

```bash
gpgconf --kill gpg-agent
```

### Testing GPG

Test everything works as needed by doing:

```bash
echo "testing GPG" | gpg --clearsign
```

You should see no errors and a plaintest message you've inputted along with a signature.

## Textlint

```bash
npm install -g textlint \
  textlint-plugin-latex \
  textlint-rule-abbr-within-parentheses \
  textlint-rule-alex \
  textlint-rule-common-misspellings \
  textlint-rule-diacritics \
  textlint-rule-en-capitalization \
  textlint-rule-en-max-word-count \
  textlint-rule-max-comma \
  textlint-rule-no-start-duplicated-conjunction \
  textlint-rule-period-in-list-item \
  textlint-rule-rousseau \
  textlint-rule-spellcheck-tech-word \
  textlint-rule-stop-words
  textlint-rule-terminology \
  textlint-rule-unexpanded-acronym \
  textlint-rule-write-good \
  write-good
```

[1]: http://nodejs.org/
[2]: http://gruntjs.com/
[3]: http://gulpjs.com/
[4]: http://superuser.com/a/828010/117521
