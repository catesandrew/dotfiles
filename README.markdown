Ruby Mine Settings
=================

Description
-----------
  This is a git repo of the custom color, keymap, and template files that we use in RubyMine.
  
Installation
------------
  To install you need to navigate into your existing directory. Since git doesn't let you clone into an existing directory you need to init a repo and add as a remote origin as shown below.

    cd ~/Library/Preferences/RubyMine10
    git init
    git remote add origin git://github.com/factorylabs/ruby-mine-settings.git
    git fetch
    git branch master origin/master
    git checkout master

Then relaunch RubyMine


Colors
------
  **Less Vibrant Ink** - a slightly more pastel version of the Vibrant Ink Textmate theme  
  **colorblind** - think rainbows and unicorns with a warm fuzzy. :: *updated 08.19.09 with minor color adjustments*  
  
  **snowblind** - think rainbows and unicorns with a warm fuzzy, only in white.
  
Keymaps
-------
  **Textmate for Realz** - adds missing commands not found in the standard keymap provided with RM.  
  **MK** - basically the default scheme with some altered shortcuts for code completion, more to come with this one.
  
Templates
---------
  **Shoulda** - context template for shoulda
  
Codestyles
----------
  **Factory** - the standard indent settings here at factory.