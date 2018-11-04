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

modes.add_binds("ex-follow", {
  -- Yank element uri to open in an external application
  { "d", "[[Hint all links (as defined by the `follow.selectors.uri` selector) and set the primary selection to the matched elements URI, so that an external app can open it]]",
  function (w)
    w:set_mode("follow", {
      prompt = "video", selector = "uri", evaluator = "uri",
      func = function (uri)
        assert(type(uri) == "string")
        uri = string.gsub(uri, " ", "%%20")
        luakit.selection.primary = uri
        if string.match(uri, "youtube") then
          luakit.spawn(string.format("mpv --ytdl-format 'best[height=720]' '%s'", uri))
          -- This also works
          -- luakit.spawn(string.format("mpv --geometry=640x360 %s", uri ))
          w:notify("trying to play file on mpv " .. uri)
        elseif string.match(uri, "vimeo") then
          luakit.spawn(string.format("mpv %s", uri))
          w:notify("trying to play file on mpv " .. uri)
        elseif string.match(uri, "vine") then
          luakit.spawn(string.format("mpv %s", uri))
          w:notify("trying to play file on mpv " .. uri)
        elseif string.match(uri, "pdf" or "PDF") then
          luakit.spawn(string.format("~/.config/scripts/pdfFromURL %s", uri))
          w:notify("trying to read file via zathura ")
        elseif string.match(uri, "jpg") then
          luakit.spawn(string.format("feh -x %s", uri))
          w:notify("file contains jpg " )
        else
          luakit.spawn(string.format("feh -x %s.jpg", uri))
          w:notify("no jpg extension | unrecognized")
        end
      end
    })
  end },
})
