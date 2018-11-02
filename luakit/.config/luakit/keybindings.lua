local modes = require "modes"

modes.add_binds("normal", {
  { "v", "Play video in page",
  function (w)
    local view = w.view
    local uri = view.hovered_uri or view.uri
    if uri then
      luakit.spawn(string.format("mpv --geometry=640x360 %s", uri ))
    end
  end },
})
