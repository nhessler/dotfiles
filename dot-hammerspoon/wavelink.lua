-- Wave Link 3 integration via the Stream Deck plugin protocol
--
-- Wave Link 3.x removed its public WebSocket API and now only accepts
-- connections that identify themselves as Stream Deck plugins via the
-- `Origin: streamdeck://` HTTP header during the WebSocket handshake.
-- We replicate that handshake so the Hammerspoon integration keeps
-- working (including the Wave XLR's red LED indicator when muted).
--
-- Dependency: websocat (brew install websocat). Hammerspoon's built-in
-- `hs.websocket.new` cannot set custom headers, so we shell out.
--
-- See dot-claude.d/_shared/skills/nh/references/ if more context lands
-- there later; or check the Stream Deck Wave Link plugin's bundled
-- bin/plugin.js for the upstream protocol.

local M = {}

M.port = 1884
M.deviceId = nil  -- auto-discovered
M.inputId = nil   -- auto-discovered
M.isMuted = nil   -- known after first query

local WEBSOCAT_PATHS = {
  "/opt/homebrew/bin/websocat",
  "/usr/local/bin/websocat",
}

local function findWebsocat()
  for _, p in ipairs(WEBSOCAT_PATHS) do
    if hs.fs.attributes(p) then return p end
  end
  return nil
end

local WEBSOCAT = findWebsocat()
local rpcId = 0

local function log(...)
  print("[WaveLink]", ...)
end

local function nextId()
  rpcId = rpcId + 1
  return rpcId
end

-- Send a JSON-RPC request via websocat. Callback (optional) receives the
-- parsed response object.
local function rpc(method, params, callback)
  if not WEBSOCAT then
    hs.alert("websocat not installed (brew install websocat)")
    return
  end

  local payload = hs.json.encode({
    jsonrpc = "2.0",
    id      = nextId(),
    method  = method,
    params  = params or {},
  })

  -- Pipe payload via stdin. websocat flags:
  --   --no-close            don't send a Close frame on stdin EOF (otherwise
  --                         we'd race the server's response and lose it)
  --   --max-messages-rev=1  exit after one received message (our response)
  -- The trailing \n in the printf format is required: websocat is line-buffered
  -- on stdin and won't send the payload as a WS message until it sees newline
  -- or EOF — and with --no-close we avoid relying on EOF. %q in string.format
  -- produces a shell-safe quoted form for the payload.
  local cmd = string.format(
    [[printf '%%s\n' %q | %s --origin 'streamdeck://' --no-close --max-messages-rev=1 'ws://127.0.0.1:%d' 2>&1]],
    payload, WEBSOCAT, M.port
  )

  hs.task.new("/bin/sh", function(_, stdout, _)
    if not callback then return end
    local firstLine = stdout and stdout:match("([^\n]+)")
    if not firstLine then return callback(nil) end
    local ok, resp = pcall(hs.json.decode, firstLine)
    if ok then callback(resp) end
  end, {"-c", cmd}):start()
end

-- Find the Wave XLR (or other Wave device) and cache its identifiers.
local function discover(then_fn)
  rpc("getInputDevices", nil, function(resp)
    if not resp or not resp.result then
      log("Discovery failed — is Wave Link running?")
      return
    end
    for _, dev in ipairs(resp.result.inputDevices or {}) do
      if dev.isWaveDevice then
        local input = (dev.inputs or {})[1]
        if input then
          M.deviceId = dev.id
          M.inputId  = input.id
          M.isMuted  = input.isMuted
          log("Discovered:", dev.name, "input:", input.name,
              "muted:", tostring(input.isMuted))
          if then_fn then then_fn() end
          return
        end
      end
    end
    log("No Wave device found in Wave Link")
  end)
end

local function withDevice(fn)
  if M.deviceId and M.inputId then
    fn()
  else
    discover(fn)
  end
end

local function setMute(muted)
  rpc("setInputDevice", {
    id     = M.deviceId,
    inputs = { { id = M.inputId, isMuted = muted } },
  })
  M.isMuted = muted
end

function M.mute()
  withDevice(function()
    if M.isMuted then
      log("Already muted, skipping")
      return
    end
    setMute(true)
    log("Muted")
  end)
end

function M.unmute()
  withDevice(function()
    if not M.isMuted then
      log("Already unmuted, skipping")
      return
    end
    setMute(false)
    log("Unmuted")
  end)
end

function M.toggleMute()
  withDevice(function()
    setMute(not M.isMuted)
    hs.alert(M.isMuted and "🔇 Mic Muted" or "🎙️ Mic Live")
  end)
end

-- Resync state from Wave Link (e.g., after the user toggled the hardware
-- mute button, since we don't subscribe to push events).
function M.refresh()
  discover()
end

return M
