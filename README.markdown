Ruby Mine Settings
=================

Description
-----------
  This is a git repo of the custom color, keymap, and template files that we use in RubyMine.
  
Installation
------------
  To install you need to navigate into your existing directory. Since git doesn't let you clone into an existing directory you need to init a repo and add as a remote origin as shown below.

    cd ~/Library/Preferences/RubyMine30
    git init
    git remote add origin git://github.com/factorylabs/ruby-mine-settings.git
    git fetch
    git branch master origin/master
    git checkout master
    git submodule init
    git submodule update

Then relaunch RubyMine. Open the `Preferences` panel and select `External Tools`. Click on `JSLint` and within the `Edit Tool` dialog box, switch the path on the `Working directory` input field to your `HOME` path for your machine.

Every now and then it will be necessary to update some of the submodules. To do this, run the following from the command line:

    bash update

Colors
------
  **Less Vibrant Ink** - a slightly more pastel version of the Vibrant Ink Textmate theme  
  **colorblind** - think rainbows and unicorns with a warm fuzzy  
  **snowblind** - think rainbows and unicorns with a warm fuzzy, only in white  
  **bloodshot** - muted version of colorblind  
  **cataracts** - pastel on grey

**Most themes use the custom [MesloGM font](https://github.com/andreberg/Meslo-Font) which needs to be installed.**
  
Keymaps
-------
  **Textmate for Realz** - adds missing commands not found in the standard keymap provided with RM  
  **MK** - basically the default scheme with some altered shortcuts for code completion  
  **Pivotal** - based on the key bindings run by Pivots  
  **VIM** - if you have to use RubyMine, might as well roll it vim style  
  
Templates
---------
  **Shoulda** - context template for shoulda
  
Codestyles
----------
  **Factory** - the standard indent settings here at factory.
