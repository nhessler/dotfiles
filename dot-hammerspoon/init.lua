hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
  hs.reload()
  notification = hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded", autoWithdraw=true})
  notification:withdrawAfter(5)
  notification:send()
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "o", function()

-- Applications/
  hs.application.open("/Applications/1Password 7.app")
  hs.application.open("/Applications/Authy Desktop.app")
  hs.application.open("/Applications/Brave Browser.app")
  hs.application.open("/Applications/Dash.app")
  hs.application.open("/Applications/Emacs.app")
  hs.application.open("/Applications/Kiwi for Gmail.app")
  hs.application.open("Message")
  hs.application.open("/Applications/Miro.app")
  hs.application.open("/Applications/Notion.app")
  hs.application.open("/Applications/Safari.app")
  hs.application.open("/Applications/Slack.app")
  --hs.application.open("/Applications/Stocks.app")
  hs.application.open("/Applications/zoom.us.app")

-- Applications/Utilities
  hs.application.open("/Applications/Utilities/Terminal.app")

end)

function appException()

  local appName       = hs.window.focusedWindow():application():name()
  local appExceptions = {"Authy Desktop", "System Preferences", "System Settings", "FaceTime", "OpenVPN Connect"}
  local result = false

  for _i, appException in ipairs(appExceptions) do
    if appName == appException then
      result = true
    end
  end

  return result
end

function windowSizeAndMove(windowTile, padding, centerOffset)
  -- windowTile is (x, y, w, h) as a percent of screen
  local window       = hs.window.focusedWindow()
  local windowFrame  = window:frame()
  local screenFrame  = window:screen():frame()
  local expectedSize = {w=0, h=0}

  local xOffset = (centerOffset and (screenFrame.w * windowTile[3] / 2) or 0)
  local yOffset = (centerOffset and (screenFrame.h * windowTile[4] / 2) or 0)

  expectedSize.w = screenFrame.w * windowTile[3] - padding * 2
  expectedSize.h = screenFrame.h * windowTile[4] - padding * 2

  if appException() then
    local wOffset = (expectedSize.w - windowFrame.w) / 2
    local hOffset = (expectedSize.h - windowFrame.h) / 2

    windowFrame.x = screenFrame.x + screenFrame.w * windowTile[1] - xOffset + wOffset
    windowFrame.y = screenFrame.y + screenFrame.h * windowTile[2] - yOffset + hOffset
  else
    windowFrame.x = screenFrame.x + screenFrame.w * windowTile[1] - xOffset + padding
    windowFrame.y = screenFrame.y + screenFrame.h * windowTile[2] - yOffset + padding
    windowFrame.w = expectedSize.w
    windowFrame.h = expectedSize.h
  end

  window:setFrame(windowFrame)
end

windowManager = hs.hotkey.modal.new({"ctrl", "alt", "cmd"}, "w", "Window Manager")

-- Exits
for _i, key in pairs({"escape", "delete", "forwarddelete", "return"}) do
  windowManager:bind('', key, function()
    hs.alert("Windows Managed")

    windowManager:exit()
  end)
end

-- Halves
windowManager:bind('', "Right", "Right Half", function() windowSizeAndMove({1/2,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Left",  "Left Half",  function() windowSizeAndMove({  0,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Up",    "Upper Half", function() windowSizeAndMove({  0,   0,   1, 1/2}, 5) end)
windowManager:bind('', "Down",  "Lower Half", function() windowSizeAndMove({  0, 1/2,   1, 1/2}, 5) end)

-- Quarters
windowManager:bind("shift", "Up"   , "Upper Right", function() windowSizeAndMove({1/2,   0, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Right", "Lower Right", function() windowSizeAndMove({1/2, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Down",  "Lower Left",  function() windowSizeAndMove({  0, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Left",  "Upper Left",  function() windowSizeAndMove({  0,   0, 1/2, 1/2}, 5) end)

-- Thirds
windowManager:bind({"ctrl", "alt"}, "Left",  "Left Third",      function() windowSizeAndMove({  0,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Up",    "Middle Third",    function() windowSizeAndMove({1/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Right", "Right Third",     function() windowSizeAndMove({2/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Down",  "Left Two Thirds", function() windowSizeAndMove({  0,   0, 2/3, 1}, 5) end)

-- Sixths
windowManager:bind({"shift", "ctrl", "alt"}, "[",     "Upper 1st Third", function() windowSizeAndMove({  0,   0, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Left",  "Lower 1st Third", function() windowSizeAndMove({  0, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Up",    "Upper 2nd Third", function() windowSizeAndMove({1/3,   0, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Down",  "Lower 2nd Third", function() windowSizeAndMove({1/3, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "Right", "Lower 3rd Third", function() windowSizeAndMove({2/3, 1/2, 1/3, 1/2}, 5) end)
windowManager:bind({"shift", "ctrl", "alt"}, "]",     "Upper 3rd Third", function() windowSizeAndMove({2/3,   0, 1/3, 1/2}, 5) end)

-- Centers
windowManager:bind({"ctrl", "alt", "cmd"}, "Up",    "Max",      function() windowSizeAndMove({0, 0, 1, 1}, 0) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Left",  "Center S", function() windowSizeAndMove({1/2, 1/2, 1/2, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Down",  "Center M", function() windowSizeAndMove({1/2, 1/2, 2/3, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Right", "Center L", function() windowSizeAndMove({1/2, 1/2, 3/4, 3/4}, 0, true) end)
