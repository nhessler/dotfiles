-- Studio lights control
-- Toggles Nanoleaf (via Apple Shortcuts) and Elgato Key Light (via REST API)

local M = {}

M.isOn = false
M.keyLightHost = nil
M.keyLightPort = 9123

local function log(...)
  print("[StudioLights]", ...)
end

-- Elgato Key Light discovery via bonjour
local function discoverKeyLight()
  M._browser = hs.bonjour.new()
  M._browser:findServices("_elg._tcp", function(b, msg, state, service)
    if msg == "service" then
      service:resolve(5, function(svc, resolveMsg, err)
        if resolveMsg == "resolved" then
          M.keyLightHost = svc:hostname():gsub("%.$", "") -- trim trailing dot
          M.keyLightPort = svc:port()
          log("Found Key Light:", svc:name(), "at", M.keyLightHost .. ":" .. M.keyLightPort)
          b:stop()
        end
      end)
    end
  end)
end

local function keyLightSet(on)
  if not M.keyLightHost then
    log("Key Light not discovered yet")
    return
  end

  local url = "http://" .. M.keyLightHost .. ":" .. M.keyLightPort .. "/elgato/lights"
  local body = hs.json.encode({numberOfLights = 1, lights = {{on = on}}})

  hs.http.asyncPut(url, body, {["Content-Type"] = "application/json"}, function(status, response)
    if status == 200 then
      log("Key Light", on == 1 and "ON" or "OFF")
    else
      log("Key Light error:", status, response)
    end
  end)
end

-- Nanoleaf state check
local function getNanoleafState()
  local output = hs.execute('shortcuts run "Studio Lights State"')
  if output then
    local state = output:match("^%s*(.-)%s*$")
    M.isOn = (state == "Yes")
    log("Nanoleaf state:", state)
  end
end

function M.on()
  hs.shortcuts.run("Studio Lights On")
  keyLightSet(1)
  M.isOn = true
  log("All lights ON")
end

function M.off()
  hs.shortcuts.run("Studio Lights Off")
  keyLightSet(0)
  M.isOn = false
  log("All lights OFF")
end

function M.toggle()
  getNanoleafState()
  if M.isOn then
    M.off()
    hs.alert("💡 Studio Lights Off")
  else
    M.on()
    hs.alert("💡 Studio Lights On")
  end
end

-- Camera watcher: auto-toggle lights when any camera turns on/off
local function anyCameraInUse()
  for _, cam in ipairs(hs.camera.allCameras()) do
    if cam:isInUse() then return true end
  end
  return false
end

local function onCameraChange()
  local inUse = anyCameraInUse()
  log("Camera change detected, any in use:", tostring(inUse))
  if inUse and not M.isOn then
    M.on()
    hs.alert("📷 Camera on — Studio Lights On")
  elseif not inUse and M.isOn then
    M.off()
    hs.alert("📷 Camera off — Studio Lights Off")
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
  discoverKeyLight()

  -- Watch all existing cameras
  for _, camera in ipairs(hs.camera.allCameras()) do
    watchCamera(camera)
  end

  -- Watch for new cameras being added
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
