-- local log = hs.logger.new('init.lua', 'debug')

-- Use Control+` to reload Hammerspoon config
-- hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, '`', nil, function()
--   hs.reload()
-- end)

keyUpDown = function(modifiers, key)
  -- Un-comment & reload config to log each keystroke that we're triggering
  -- log.d('Sending keystroke:', hs.inspect(modifiers), key)

  hs.eventtap.keyStroke(modifiers, key, 0)
end

-- Subscribe to the necessary events on the given window filter such that the
-- given hotkey is enabled for windows that match the window filter and disabled
-- for windows that don't match the window filter.
--
-- windowFilter - An hs.window.filter object describing the windows for which
--                the hotkey should be enabled.
-- hotkey       - The hs.hotkey object to enable/disable.
--
-- Returns nothing.
enableHotkeyForWindowsMatchingFilter = function(windowFilter, hotkey)
  windowFilter:subscribe(hs.window.filter.windowFocused, function()
    hotkey:enable()
  end)

  windowFilter:subscribe(hs.window.filter.windowUnfocused, function()
    hotkey:disable()
  end)
end

local secrets = require('secrets')
secrets.start('.secrets.json')

Config = {}
Config.applications = {
  -- 'z', was saved for menu pop
  -- 'x', no action
  ['org.acates.EmacsClient'] = {
    bundleID = 'org.acates.EmacsClient',
    hyper_key = 'c',
    tags = {'editor'},
    rules = {
      {nil, 2, hs.layout.maximized}
    }
  },
  ['org.vim.MacVim'] = {
    bundleID = 'org.vim.MacVim',
    hyper_key = 'v',
  },
  ['com.google.Chrome'] = {
    bundleID = 'com.google.Chrome',
    hyper_key = 'b',
    rules = {
      {nil, 1, hs.layout.maximized},
      {"Confluence", 1, hs.layout.maximized},
      {"Meet - ", 2, hs.layout.maximized},
    }
  },
  -- 'n', no action
  ['com.freron.MailMate'] = {
    bundleID = 'com.freron.MailMate',
    hyper_key = 'm',
    tags = {'communication'},
    rules = {
      {nil, 2, hs.layout.maximized}
    }
  },
  -- ',', no action
  -- '.', no action
  -- '/', no action

  -- 'a', no action
  ['com.sonos.macController'] = {
    bundleID = 'com.sonos.macController',
    hyper_key = 's',
  },
  ['com.kapeli.dashdoc'] = {
    bundleID = 'com.kapeli.dashdoc',
    local_bindings = {'d'},
    tags = {'coding'}
  },
  ['com.apple.finder'] = {
    bundleID = 'com.apple.finder',
    hyper_key = 'f'
  },
  -- 'g', no action
  -- 'h', saved for launchbar
  -- 'j', no action
  ['com.flexibits.fantastical2.mac'] = {
    bundleID = 'com.flexibits.fantastical2.mac',
    hyper_key = 'k',
    local_bindings = {']', 'r'},
    tags = {'planning', 'review', 'calendar'},
    whitelisted = true,
    rules = {
      {nil, 2, hs.layout.maximized}
    }
  },
  ['com.tinyspeck.slackmacgap'] = {
    bundleID = 'com.tinyspeck.slackmacgap',
    hyper_key = 'l',
    tags = {'communication'},
    rules = {
      {nil, 2, hs.layout.maximized}
    }
  },
  -- ';', no action
  -- ''', no action

  -- 'q', saved for movewindows
  -- 'w', saved for moom
  ['com.manytricks.Moom'] = {
    bundleID = 'com.manytricks.Moom',
    local_bindings = {'w'},
  },
  ['org.gnu.Emacs'] = {
    bundleID = 'org.gnu.Emacs',
    hyper_key = 'e',
    tags = {'editor'},
    rules = {
      {nil, 2, hs.layout.maximized}
    }
  },
  -- 'r', saved for fantastical
  ['com.googlecode.iterm2'] = {
    bundleID = 'com.googlecode.iterm2',
    hyper_key = 't',
    -- hyper_key = 't',
    tags = {'coding'},
    rules = {
      {nil, 1, hs.layout.maximized}
    }
  },
  -- 'y', no action
  ['uk.co.tla-systems.pcalc'] = {
    bundleID = 'uk.co.tla-systems.pcalc',
    hyper_key = 'u'
  },
  ['com.apple.iChat'] = { -- maybe 'com.apple.MobileSMS'
    bundleID = 'com.apple.iChat',
    hyper_key = 'i',
    tags = {'communication', 'distraction'},
    rules = {
      {nil, 2, hs.layout.right30}
    }
  },
  -- 'o', no action
  -- 'p', saved for snippets lab
  ['com.renfei.SnippetsLab'] = {
    bundleID = 'com.renfei.SnippetsLab',
    local_bindings = {'p'},
    tags = {'coding'}
  },
  ['com.1password.1password'] = {
    bundleID = 'com.1password.1password',
    hyper_key = '1'
  },

  ['Qisda.DDPM'] = {
    bundleID = 'Qisda.DDPM',
    local_bindings = {'[', ']'},
  },
  -- '[', no action
  -- ']', no action
  --- no hyper-key
  ['com.figma.Desktop'] = {
    bundleID = 'com.figma.Desktop',
    tags = {'design'},
    rules = {
      {nil, 1, hs.layout.maximized}
    }
  },
  ['com.runningwithcrayons.Alfred'] = {
    bundleID = 'com.runningwithcrayons.Alfred',
    local_bindings = {'space', 'o'}
  },
  ['com.ideasoncanvas.mindnode.macos'] = {
    bundleID = 'com.ideasoncanvas.mindnode.macos',
    tags = {'research'},
    rules = {
      {nil, 1, hs.layout.maximized}
    }
  },
  ['com.reederapp.5.macOS'] = {
    bundleID = 'com.reederapp.5.macOS',
    tags = {'distraction'},
    rules = {
      {nil, 1, hs.layout.maximized}
    }
  },
  ['md.obsidian'] = {
    bundleID = 'md.obsidian',
    tags = {'research', 'notes'},
    rules = {
      {nil, 1, hs.layout.maximized}
    }
  },
  ['us.zoom.xos'] = {
    bundleID = 'us.zoom.xos',
    rules = {
      {"Zoom Meeting", 2, hs.layout.maximized}
    }
  }
}

-- provide the ability to override config per computer
if (hs.fs.displayName('./local_config.lua')) then
  require('local_config')
end

-- configure spaces for headspace
Config.spaces = {}
Config.funcs = {}

SpaceCadet = require('space-cadet')
SpaceCadet.start(Config)

-- require('markdown')
-- require('super')
-- require('windows')

hs.notify.new({title='Hammerspoon', informativeText='Ready to rock 🤘'}):send()
