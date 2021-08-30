# Load the local customizations in autoconfig.yml, if any, overriding the
# definitions above
config.load_autoconfig()

c.auto_save.session = True

# Do not autoplay videos
c.content.autoplay = False

c.completion.height = "25%"
c.completion.quick = True
c.completion.shrink = True
c.completion.use_best_match = True

# Time (in ms) to wait before removing finished downloads
c.downloads.remove_finished = 30000

c.input.partial_timeout = 0

c.tabs.background = True

c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "d":       "https://duckduckgo.com/?q={}",
    "g":       "https://google.com/search?q={}",
    "w":       "https://en.wikipedia.org/wiki/{}",

    "az":      "https://amazon.ca/s/?field-keywords={}",
    "gh":      "https://github.com/search?q={}",
    "yt":      "https://youtube.com/results?search_query={}",

    "def":     "https://merriam-webster.com/dictionary/{}",

    "arch":    "https://wiki.archlinux.org/index.php?search={}"
}

c.bindings.commands["normal"] = {
    "T": "set-cmd-text -s :tab-select",

    "gt": "tab-next",
    "gT": "tab-prev",
    "<<": "tab-move -",
    ">>": "tab-move +",

    ",m": "hint links spawn umpv {hint-url}",
    ",M": "hint --rapid links spawn umpv {hint-url}"
}
