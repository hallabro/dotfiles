require "lfs"

-- Check for lua configuration files that will never be loaded because they are
-- shadowed by builtin modules.
table.insert(package.loaders, 2, function (modname)
    if not package.searchpath then return end
    local f = package.searchpath(modname, package.path)
    if not f or f:find(luakit.install_paths.install_dir .. "/", 0, true) ~= 1 then
        return
    end
    local lf = luakit.config_dir .. "/" .. modname:gsub("%.","/") .. ".lua"
    if f == lf then
        msg.warn("Loading local version of '" .. modname .. "' module: " .. lf)
    elseif lfs.attributes(lf) then
        msg.warn("Found local version " .. lf
            .. " for core module '" .. modname
            .. "', but it won't be used, unless you update 'package.path' accordingly.")
    end
end)

require "unique_instance"

luakit.process_limit = 4
soup.cookies_storage = luakit.data_dir .. "/cookies.db"

local lousy = require "lousy"

lousy.theme.init(lousy.util.find_config("theme.lua"))
assert(lousy.theme.get(), "failed to load theme")

local window = require "window"
local webview = require "webview"
local log_chrome = require "log_chrome"

window.add_signal("build", function (w)
    local widgets, l, r = require "lousy.widget", w.sbar.l, w.sbar.r

    -- Left-aligned status bar widgets
    l.layout:pack(widgets.uri())
    l.layout:pack(widgets.hist())
    l.layout:pack(widgets.progress())

    -- Right-aligned status bar widgets
    r.layout:pack(widgets.buf())
    r.layout:pack(log_chrome.widget())
    r.layout:pack(widgets.ssl())
    r.layout:pack(widgets.tabi())
    r.layout:pack(widgets.scroll())
end)

local modes = require "modes"
local binds = require "binds"
local settings = require "settings"
require "settings_chrome"
local adblock = require "adblock"
local adblock_chrome = require "adblock_chrome"
local webinspector = require "webinspector"
local quickmarks = require "quickmarks"
local undoclose = require "undoclose"
local tabhistory = require "tabhistory"
local userscripts = require "userscripts"
local bookmarks = require "bookmarks"
local bookmarks_chrome = require "bookmarks_chrome"
local downloads = require "downloads"
local downloads_chrome = require "downloads_chrome"
local viewpdf = require "viewpdf"
local follow = require "follow"
local cmdhist = require "cmdhist"
local search = require "search"
local taborder = require "taborder"
local history = require "history"
local history_chrome = require "history_chrome"
local help_chrome = require "help_chrome"
local binds_chrome = require "binds_chrome"
local completion = require "completion"
local open_editor = require "open_editor"
local error_page = require "error_page"
local styles = require "styles"
local hide_scrollbars = require "hide_scrollbars"
local image_css = require "image_css"
local newtab_chrome = require "newtab_chrome"
local tab_favicons = require "tab_favicons"
local view_source = require "view_source"
local follow_selected = require "follow_selected"
local go_input = require "go_input"
local go_next_prev = require "go_next_prev"
local go_up = require "go_up"
local select = require "select"
require "noscript"

require_web_module("referer_control_wm")

downloads.add_signal("open-file", function (file)
    luakit.spawn(string.format("xdg-open %q", file))
    return true
end)

select.label_maker = function ()
    local chars = charset("asdfqwerzxcv")
    return trim(sort(reverse(chars)))
end

;(function ()
    settings.set_setting("window.scroll_step", 250)
end)()

;(function ()
    settings.set_setting("webview.enable_javascript", false)
    settings.set_setting("webview.enable_java", false)
    settings.set_setting("webview.enable_plugins", false)

    local script_enabled_domains = {
        "boards.4chan.org",
        "github.com",
        "imgur.com",
        "old.reddit.com",
    }

    for _,v in pairs(script_enabled_domains) do
        --settings.set_setting("webview.enable_javascript", true, { domain = v })
        settings.on[v].webview.enable_javascript = true
    end
end)()

require "keybindings"

settings.window.search_engines.searx = "https://searx.me/?q=%s"
settings.window.search_engines.default = settings.window.search_engines.searx

follow.stylesheet = follow.stylesheet .. [===[
    #luakit_select_overlay .hint_label {
        font-size: 18px;
	opacity: 1;
    }
]===]

window.new(uris)
