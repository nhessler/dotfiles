-- Camera watcher
-- Fires registered callbacks when any camera turns on or off

local M = {}

M._onCallbacks = {}
M._offCallbacks = {}
M._inUse = false

local function log(...)
  print("[CameraWatcher]", ...)
end

function M.onCameraOn(fn)
  table.insert(M._onCallbacks, fn)
end

function M.onCameraOff(fn)
  table.insert(M._offCallbacks, fn)
end

local function anyCameraInUse()
  for _, cam in ipairs(hs.camera.allCameras()) do
    if cam:isInUse() then return true end
  end
  return false
end

local function onCameraChange()
  local inUse = anyCameraInUse()
  log("Camera change detected, any in use:", tostring(inUse))

  if inUse and not M._inUse then
    M._inUse = true
    hs.alert("📷 Camera On")
    for _, fn in ipairs(M._onCallbacks) do fn() end
  elseif not inUse and M._inUse then
    M._inUse = false
    hs.alert("📷 Camera Off")
    for _, fn in ipairs(M._offCallbacks) do fn() end
  end
end

local function watchCamera(camera)
  if camera:isPropertyWatcherRunning() then
    camera:stopPropertyWatcher()
  end
  camera:setPropertyWatcherCallback(function()
    onCameraChange()
  end)
  camera:startPropertyWatcher()
  log("Watching camera:", camera:name())
end

function M.init()
  for _, camera in ipairs(hs.camera.allCameras()) do
    watchCamera(camera)
  end

  hs.camera.setWatcherCallback(function(camera, state)
    log("Camera", state .. ":", camera:name())
    if state == "Added" then
      watchCamera(camera)
    end
    onCameraChange()
  end)
  hs.camera.startWatcher()
end

return M
