hs.hotkey.bind({"cmd", "alt", "ctrl"}, "r", function()
  hs.reload()
  notification = hs.notify.new({title="Hammerspoon", informativeText="Config Reloaded", autoWithdraw=true})
  notification:withdrawAfter(5)
  notification:send()
end)

function windowMove(windowTile, padding, centerOffset)
  local window      = hs.window.focusedWindow()
  local windowFrame = window:frame()
  local screenFrame = window:screen():frame()
  local xOffset = (centerOffset and (screenFrame.w * windowTile[3] / 2) or 0)
  local yOffset = (centerOffset and (screenFrame.h * windowTile[4] / 2) or 0)


  windowFrame.x = screenFrame.x + screenFrame.w * windowTile[1] - xOffset + padding
  windowFrame.y = screenFrame.y + screenFrame.h * windowTile[2] - yOffset + padding
  windowFrame.w = screenFrame.w * windowTile[3] - padding * 2
  windowFrame.h = screenFrame.h * windowTile[4] - padding * 2

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
windowManager:bind('', "Right", "Right Half", function() windowMove({1/2,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Left",  "Left Half",  function() windowMove({  0,   0, 1/2,   1}, 5) end)
windowManager:bind('', "Up",    "Upper Half", function() windowMove({  0,   0,   1, 1/2}, 5) end)
windowManager:bind('', "Down",  "Lower Half", function() windowMove({  0, 1/2,   1, 1/2}, 5) end)

-- Quarters
windowManager:bind("shift", "Up"   , "Upper Right", function() windowMove({1/2,   0, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Right", "Lower Right", function() windowMove({1/2, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Down",  "Lower Left",  function() windowMove({  0, 1/2, 1/2, 1/2}, 5) end)
windowManager:bind("shift", "Left",  "Upper Left",  function() windowMove({  0,   0, 1/2, 1/2}, 5) end)

-- Thirds

windowManager:bind({"ctrl", "alt"}, "Up",    "Middle Third",    function() windowMove({1/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Right", "Right Third",     function() windowMove({2/3,   0, 1/3, 1}, 5) end)
windowManager:bind({"ctrl", "alt"}, "Down",  "Left Two Thirds", function() windowMove({  0,   0, 2/3, 1}, 5) end)

-- Centers
windowManager:bind({"ctrl", "alt", "cmd"}, "Up",    "Max",      function() windowMove({0, 0, 1, 1}, 0) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Left",  "Center S", function() windowMove({1/2, 1/2, 1/2, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Down",  "Center M", function() windowMove({1/2, 1/2, 2/3, 3/4}, 0, true) end)
windowManager:bind({"ctrl", "alt", "cmd"}, "Right", "Center L", function() windowMove({1/2, 1/2, 3/4, 3/4}, 0, true) end)
