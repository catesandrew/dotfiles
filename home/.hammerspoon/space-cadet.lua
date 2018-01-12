-- Load Extensions
local application = require "hs.application"
local window = require "hs.window"
local hotkey = require "hs.hotkey"
local keycodes = require "hs.keycodes"
local fnutils = require "hs.fnutils"
local alert = require "hs.alert"
local screen = require "hs.screen"
local grid = require "hs.grid"
local osascript = require "hs.osascript"
local hints = require "hs.hints"
local appfinder = require "hs.appfinder"

appWorkflow = [[
  set appName to "%s"
  set startIt to false
  tell application "System Events"
    if not (exists process appName) then
      set startIt to true
    else if frontmost of process appName then
      set visible of process appName to false
    else
      set frontmost of process appName to true
    end if
  end tell
  if startIt then
    tell application appName to activate
  end if
]]

launchAppleScript = function(appName)
  str = string.format(appWorkflow, appName)
  osascript.applescript(str)
end

launchSingle = function(appname)
  hs.application.launchOrFocus(appname)
end

oascripts = {
  -- {'z', ''}, was saved for menu pop
  {'x', 'Safari'},
  {'c', 'Google Chrome Canary'},
  {'v', 'MacVim'},
  {'b', 'Google Chrome'},
  {'n', 'NotePlan'},
  {'m', 'MailMate'},
  {'a', 'BusyContacts'},
  {'s', 'Sonos'},
  -- {'d', ''}, saved for dash
  {'f', 'Finder'},
  {'g', 'Tower'},
  -- {'h', ''}, saved for launchbar
  {'j', 'Jump Desktop'},
  -- {'k', 'Fantastical 2'},
  -- {'l', ''}, no action
  -- {';', ''}, daylite
  {'q', 'Quiver'},
  -- {'w', ''}, saved for moom
  {'e', 'Emacs'},
  -- {'r', ''}, saved for fantastical 2
  -- {'t', 'iTerm'},
  -- {'y', ''}, no action
  {'u', 'Calcbot'},
  {'i', 'Messages'},
  {'o', 'OmniFocus'},
  -- {'p', ''}, saved for snippets lab
}

for i, app in ipairs(oascripts) do
  hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, app[1], function()
      launchAppleScript(app[2])
  end)
end

-- double identity apps

doubleapps = {
  {'t', 'iTerm2', 'iTerm'},
  {'k', 'Fantastical', 'Fantastical 2'},
}

launchDouble = function(appName1, appName2)
  local app = hs.application.find(appName1)
  if (app and hs.application.isRunning(app)) then
    -- alert(string.format('found running app: %s', appName1))
    launchAppleScript(appName1)
  else
    -- alert(string.format('launching app: %s', appName2))
    hs.application.launchOrFocus(appName2)
  end
end

for i, app in ipairs(doubleapps) do
  hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, app[1], function()
      launchDouble(app[2], app[3])
  end)
end
