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

-- Watch for Minimizing Windows and/or Applications

local appWatcherStarted = false
local appWatches = {}

-- Toggle an application between being the frontmost app, and being hidden
function toggleApplication(appName)
    local app = hs.appfinder.appFromName(appName)
    if not app then
        return
    end

    local mainWin = app:mainWindow()
    if mainWin then
        if mainWin == hs.window.focusedWindow() then
            mainWin:application():hide()
        else
            mainWin:application():activate(true)
            mainWin:application():unhide()
            mainWin:focus()
        end
    end
end

local function manageWindow(win, app)
  if not win:isStandard() then return end

  -- only trigger on focused window movements otherwise the reshuffling triggers itself
  local newWatch = win:newWatcher(function(el,ev,wat,ud)
      if el == app:focusedWindow() then
          -- reshuffle(app)
      end
  end)

  newWatch:start({hs.uielement.watcher.windowMoved, hs.uielement.watcher.windowResized, hs.uielement.watcher.elementDestroyed})
  local redrawWatch = win:newWatcher(function (el,ev,wat,ud)
      -- drawTabs(app)
  end)

  redrawWatch:start({hs.uielement.watcher.elementDestroyed, hs.uielement.watcher.titleChanged})
end

local function watchApp(app)
  for i,win in ipairs(app:allWindows()) do
    manageWindow(win, app)
  end

  local winWatch = app:newWatcher(function(el,ev,wat,appl)
      manageWindow(el,appl)
  end, app)
  winWatch:start({hs.uielement.watcher.windowCreated})

  local redrawWatch = app:newWatcher(function (el,ev,wat,ud)
      _animationDuration = hs.window.animationDuration
      hs.window.animationDuration = 0
      el:unminimize()
      launchAppleScript(el:application():name())
      hs.window.animationDuration = _animationDuration
  end)

  redrawWatch:start({hs.uielement.watcher.windowMinimized})
  -- reshuffle(app)
end

function enableForApp(app)
  if type(app) == 'string' then
    appWatches[app] = true
    app = hs.application.get(app)
  end

  -- might already be running
  if app then
    watchApp(app)
  end

  -- set up a watcher to catch any watched app launching or terminating
  if appWatcherStarted then return end
  appWatcherStarted = true

  local appWatcher = hs.application.watcher.new(function(appName, eventType, appObject)
    if (eventType == hs.application.watcher.activated) then
      -- Bring all Finder windows forward when one gets activated
      if (appName == 'Finder') then
        appObject:selectMenuItem({'Window', 'Bring All to Front'})
      end
    elseif (eventType == hs.application.watcher.launched and appWatches[appName]) then
      watchApp(appObject)
    elseif (eventType == hs.application.watcher.terminated) then
      -- trashTabs(appObject:pid())
    end
  end)
  appWatcher:start()
end


-- Previous Seil Keybindings
-- Caps Lock (51) → Left Control (59)
-- Left Control (59) → F19 (80)

-- Brett's Binding
-- Caps Lock → F18
-- A global variable for the Hyper Mode
k = hotkey.modal.new({}, "F17")

-- Trigger existing hyper key shortcuts
-- Hyper+key for all the below are setup somewhere
hyperBindings = {'z', 'd', 'h', 'c', 'l', 'r', 'y', 'p', ';', 'w'}

for i,key in ipairs(hyperBindings) do
  k:bind({}, key, nil, function() hs.eventtap.keyStroke({'cmd','alt','shift','ctrl'}, key)
    k.triggered = true
  end)
end

-- build our own

launchSingle = function(appname)
  hs.application.launchOrFocus(appname)
  k.triggered = true
end

-- Single keybinding for app launch
singleapps = {
  -- {'z', ''}, was saved for menu pop
  {'x', 'Safari'},
  -- {'c', ''}, no action
  -- {'v', ''}, macvim
  -- {'b', ''}, google chrome canary
  -- {'n', ''}, napkin
  -- {'m', ''}, mailmate
  -- {'a', ''}, busy contacts
  -- {'s', ''}, sonos
  -- {'d', ''}, saved for dash
  -- {'f', ''}, finder
  -- {'g', ''}, tower
  -- {'h', ''}, saved for launchbar
  -- {'j', ''}, jump desktop
  -- {'k', ''}, fantastical
  -- {'l', ''}, no action
  -- {';', ''}, daylite
  -- {'q', ''}, quiver
  -- {'w', ''}, saved for moom
  -- {'e', ''}, emacs
  -- {'r', ''}, saved for fantastical 2
  -- {'t', ''}, iTerm
  -- {'y', ''}, no action
  -- {'u', ''}, calcbot
  -- {'i', ''}, messages
  -- {'o', ''}, omnifocus
  -- {'p', ''}, saved for snippets lab
}

for i, app in ipairs(singleapps) do
  k:bind({}, app[1], function() launchSingle(app[2]); k:exit(); end)
end

-- apple scripts

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
  k.triggered = true
end

oascripts = {
  -- {'z', ''}, was saved for menu pop
  -- {'x', ''}, safari
  {'c', 'Google Chrome Canary'},
  {'v', 'MacVim'},
  {'b', 'Google Chrome'},
  {'n', 'Napkin'},
  {'m', 'MailMate'},
  {'a', 'BusyContacts'},
  {'s', 'Sonos'},
  -- {'d', ''}, saved for dash
  {'f', 'Finder'},
  {'g', 'Tower'},
  -- {'h', ''}, saved for launchbar
  {'j', 'Jump Desktop'},
  {'k', 'Fantastical 2'},
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
  k:bind({}, app[1], function() launchAppleScript(app[2]); k:exit(); end)
end

for i, app in ipairs(oascripts) do
  k:bind({}, app[1], function() launchAppleScript(app[2]); k:exit(); end)
  enableForApp(app[2])
end

-- double identity apps

doubleapps = {
  {'t', 'iTerm2', 'iTerm'},
}

launchDouble = function(appName1, appName2)
  local app = hs.application.find(appName1)
  if (app and hs.application.isRunning(app)) then
    launchAppleScript(appName1)
  else
    hs.application.launchOrFocus(appName2)
    k.triggered = true
  end
end

for i, app in ipairs(doubleapps) do
  k:bind({}, app[1], function() launchDouble(app[2], app[3]); k:exit(); end)
  enableForApp(app[2])
end

-- Sequential keybindings, e.g. Hyper-a,f for Finder
-- a = hs.hotkey.modal.new({}, "F16")
-- apps = {
--   {'d', 'Twitter'},
--   {'f', 'Finder'},
--   {'s', 'Skype'},
-- }
-- for i, app in ipairs(apps) do
--   a:bind({}, app[1], function() launch(app[2]); a:exit(); end)
-- end
--
-- pressedA = function() a:enter() end
-- releasedA = function() end
-- k:bind({}, 'a', nil, pressedA, releasedA)

-- Enter Hyper Mode when F18 (Hyper/Capslock) is pressed
-- pressedF18 = function()
--   k.triggered = false
--   k:enter()
-- end
--
-- -- Leave Hyper Mode when F18 (Hyper/Capslock) is pressed,
-- --   send ESCAPE if no other keys are pressed.
-- releasedF18 = function()
--   k:exit()
--   if not k.triggered then
--     hs.eventtap.keyStroke({}, 'ESCAPE')
--   end
-- end

-- Bind the Hyper key
-- f18 = hotkey.bind({}, 'F18', pressedF18, releasedF18)


 -- Enter Hyper Mode when F19 (hyper/left control) is pressed
pressedF19 = function()
  k.triggered = false
  k:enter()
end

-- Leave Hyper Mode when F19 (hyper/left control) is pressed,
--   send ESCAPE if no other keys are pressed.
releasedF19 = function()
  k:exit()
  if not k.triggered then
    hs.eventtap.keyStroke({}, 'ESCAPE')
  end
end

--

-- lc = hotkey.modal.new({}, 'F17')
-- leftCtrlBindings = {'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';'}
--
-- for i,key in ipairs(hyperBindings) do
--   lc:bind({}, key, nil, function() hs.eventtap.keyStroke({'ctrl'}, key)
--     lc.triggered = true
--   end)
-- end
--
-- -- Enter when LeftCtrl (Capslock) is pressed
-- pressedLeftCtrl = function()
--   lc.triggered = false
--   lc:enter()
-- end
--
-- -- Leave when LeftCtrl (Capslock) is pressed,
-- --   send ESCAPE if no other keys are pressed.
-- releasedLeftCtrl = function()
--   lc:exit()
--   if not lc.triggered then
--     hs.eventtap.keyStroke({}, 'ESCAPE')
--   end
-- end

-- Bind the Hyper key
function createHyper()
  f19 = hotkey.bind({}, 'F19', pressedF19, releasedF19)
  -- leftControl = hotkey.bind({}, 59, 'left control', pressedLeftCtrl, releasedLeftCtrl)
end



-- Cursor locator

local mouseCircle = nil
local mouseCircleTimer = nil

function mouseHighlight()
  size = 150
    -- Delete an existing highlight if it exists
    if mouseCircle then
        mouseCircle:delete()
        mouseCircle2:delete()
        if mouseCircleTimer then
            mouseCircleTimer:stop()
        end
    end
    -- Get the current co-ordinates of the mouse pointer
    mousepoint = hs.mouse.getAbsolutePosition()
    -- Prepare a big red circle around the mouse pointer
    mouseCircle = hs.drawing.circle(hs.geometry.rect(mousepoint.x-(size/2), mousepoint.y-(size/2), size, size))
    mouseCircle2 = hs.drawing.circle(hs.geometry.rect(mousepoint.x-(size/4), mousepoint.y-(size/4), size/2, size/2))
    mouseCircle:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle2:setStrokeColor({["red"]=1,["blue"]=0,["green"]=0,["alpha"]=1})
    mouseCircle:setFill(false)
    mouseCircle2:setFill(false)
    mouseCircle:setStrokeWidth(3)
    mouseCircle2:setStrokeWidth(5)
    mouseCircle:show()
    mouseCircle2:show()

    -- Set a timer to delete the circle after 3 seconds
    mouseCircleTimer = hs.timer.doAfter(1, function() mouseCircle:delete() mouseCircle2:delete() end)
end
-- hotkey.bind({"cmd","alt","shift"}, "D", mouseHighlight)






local tabs = require "tabs"

local definitions = nil
local hyper = nil
local hyper2 = nil

local gridset = function(frame)
	return function()
		local win = window.focusedWindow()
		if win then
			grid.set(win, frame, win:screen())
		else
			alert.show("No focused window.")
		end
	end
end

auxWin = nil
function saveFocus()
  auxWin = window.focusedWindow()
  alert.show("Window '" .. auxWin:title() .. "' saved.")
end
function focusSaved()
  if auxWin then
    auxWin:focus()
  end
end

local hotkeys = {}

function createHotkeys()
  for key, fun in pairs(definitions) do
    local mod = hyper
    if string.len(key) == 2 and string.sub(key,2,2) == "c" then
      mod = {"cmd"}
    elseif string.len(key) == 2 and string.sub(key,2,2) == "l" then
      mod = {"ctrl"}
    end

    local hk = hotkey.new(mod, string.sub(key,1,1), fun)
    table.insert(hotkeys, hk)
    hk:enable()
  end
end

function rebindHotkeys()
  for i, hk in ipairs(hotkeys) do
    hk:disable()
  end
  hotkeys = {}
  createHotkeys()
  alert.show("Rebound Hotkeys")
end

function applyPlace(win, place)
  local scrs = screen:allScreens()
  local scr = scrs[place[1]]
  grid.set(win, place[2], scr)
end

function applyLayout(layout)
  return function()
    for appName, place in pairs(layout) do
      local app = appfinder.appFromName(appName)
      if app then
        for i, win in ipairs(app:allWindows()) do
          applyPlace(win, place)
        end
      end
    end
  end
end

function init()
  createHyper()
  -- createHotkeys()
  -- keycodes.inputSourceChanged(rebindHotkeys)
  -- tabs.enableForApp("Emacs")

  alert.show("Hammerspoon, at your service.")
end

-- Actual config =================================

hyper = {"cmd", "alt", "ctrl","shift"}
hyper2 = {"ctrl"}
hs.window.animationDuration = 0;
-- hints.style = "vimperator"
-- Set grid size.
grid.GRIDWIDTH  = 6
grid.GRIDHEIGHT = 8
grid.MARGINX = 0
grid.MARGINY = 0
local gw = grid.GRIDWIDTH
local gh = grid.GRIDHEIGHT

local gomiddle = {x = 1, y = 1, w = 4, h = 6}
local goleft = {x = 0, y = 0, w = gw/2, h = gh}
local goright = {x = gw/2, y = 0, w = gw/2, h = gh}
local gobig = {x = 0, y = 0, w = gw, h = gh}

local fullApps = {
  "Safari","Aurora","Nightly","Xcode","Qt Creator","Google Chrome",
  "Google Chrome Canary", "Eclipse", "Coda 2", "iTunes", "Emacs", "Firefox"
}
local layout2 = {
  Airmail = {1, gomiddle},
  Spotify = {1, gomiddle},
  Calendar = {1, gomiddle},
  Dash = {1, gomiddle},
  iTerm = {2, goright},
  MacRanger = {2, goleft},
}
fnutils.each(fullApps, function(app) layout2[app] = {1, gobig} end)

definitions = {
  [";"] = saveFocus,
  a = focusSaved,

  h = gridset(gomiddle),
  t = gridset(goleft),
  n = grid.maximizeWindow,
  s = gridset(goright),

  g = applyLayout(layout2),

  d = grid.pushWindowNextScreen,
  r = hs.reload,
  q = function() appfinder.appFromName("Hammerspoon"):kill() end,

  k = function() hints.windowHints(appfinder.appFromName("Emacs"):allWindows()) end,
  j = function() hints.windowHints(window.focusedWindow():application():allWindows()) end,
  ll = function() hyper, hyper2 = hyper2,hyper; rebindHotkeys() end,
  ec = function() hints.windowHints(nil) end
}

-- launch and focus applications
fnutils.each({
  { key = "o", app = "MacRanger" },
  { key = "e", app = "Google Chrome" },
  { key = "u", app = "Emacs" },
  { key = "i", app = "iTerm2" },
  { key = "m", app = "Airmail" }
}, function(object)
    definitions[object.key] = function()
      local app = appfinder.appFromName(object.app)
      if app then app:activate() end
    end
end)

for i=1,6 do
  definitions[tostring(i)] = function()
    local app = application.frontmostApplication()
    tabs.focusTab(app,i)
  end
end

init()
