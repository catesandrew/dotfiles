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

[1]: http://nodejs.org/
[2]: http://gruntjs.com/
[3]: http://gulpjs.com/
[4]: http://superuser.com/a/828010/117521
