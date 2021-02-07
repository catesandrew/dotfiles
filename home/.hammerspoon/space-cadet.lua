-- HYPER
--
-- Hyper is a hyper shortcut modal.
--
-- Feel free to modify... I use karabiner-elements.app on my laptop and QMK on
-- my mech keyboards to bind a single key to `F19`, which fires all this
-- Hammerspoon-powered OSX automation.
--
-- I chiefly use it to launch applications quickly from a single press,
-- although I also use it to create "universal" local bindings inspired by
-- Shawn Blanc's OopsieThings.
-- https://thesweetsetup.com/oopsiethings-applescript-for-things-on-mac/

-- Load Extensions

-- local log = hs.logger.new('space-cadet.lua', 'debug')

local hyper = hs.hotkey.modal.new({}, nil)

hyper.pressed  = function() hyper:enter() end
hyper.released = function() hyper:exit() end

-- Set the key you want to be HYPER to F19 in karabiner or keyboard
-- Bind the Hyper key to the hammerspoon modal
hs.hotkey.bind({}, 'F19', hyper.pressed, hyper.released)

hyper.launch = function(app)
  hs.application.launchOrFocusByBundleID(app.bundleID)
end

appWorkflow = [[
  set processIsRunning to true
  tell application "System Events"
    set runningProcesses to processes whose bundle identifier is "%s"
    if runningProcesses is {} then
      tell application id "%s" to activate
    else
      set myProcess to first process whose bundle identifier is "%s"
      if frontmost of myProcess is true then
        set visible of myProcess to false
      else
        set frontmost of myProcess to true
        --  bring a window to frontmost without focusing it
        -- perform action "AXRaise" of window 1 of myProcess
      end if
    end if
  end tell
]]

hyper.launchAppleScript = function(app)
  str = string.format(appWorkflow, app.bundleID, app.bundleID, app.bundleID)
  hs.osascript.applescript(str)
end

-- Expects a configuration table with an applications key that has the
-- following form:
-- config_table.applications = {
--   ['com.culturedcode.ThingsMac'] = {
--     bundleID = 'com.culturedcode.ThingsMac',
--     hyper_key = 't',
--     tags = {'planning', 'review'},
--     local_bindings = {',', '.'}
--   },
-- }
hyper.start = function(config_table)
  -- Use the hyper key with the application config to use the `hyper_key`
  hs.fnutils.map(config_table.applications, function(app)
    -- Apps that I want to jump to
    if app.hyper_key then
      -- hyper:bind({}, app.hyper_key, function() hyper.launchAppleScript(app); end)
      hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, app.hyper_key, function() hyper.launchAppleScript(app); end)
    end

    -- I use hyper to power some shortcuts in different apps If the app is closed
    -- and I press the shortcut, open the app and send the shortcut, otherwise
    -- just send the shortcut.
    if app.local_bindings then
      hs.fnutils.map(app.local_bindings, function(key)
        -- hyper:bind({}, key, nil, function()
        hs.hotkey.bind({'shift', 'ctrl', 'alt', 'cmd'}, key, nil, function()
          if hs.application.get(app.bundleID) then
            hs.eventtap.keyStroke({'cmd','alt','shift','ctrl'}, key)
          else
            hyper.launchAppleScript(app)
            hs.timer.waitWhile(
              function()
                return not hs.application.get(app.bundleID):isFrontmost()
              end,
              function()
                hs.eventtap.keyStroke({'cmd','alt','shift','ctrl'}, key)
              end
            )
          end
        end)
      end)
    end
  end)
end

return hyper
