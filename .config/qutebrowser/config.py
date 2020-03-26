# Load the local customizations in autoconfig.yml, if any, overriding the
# definitions above
config.load_autoconfig()

c.completion.shrink = True
c.completion.use_best_match = True

c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "d":       "https://duckduckgo.com/?q={}",
    "g":       "https://google.com/search?q={}",
    "w":       "https://en.wikipedia.org/wiki/{}",

    "az":      "https://amazon.ca/s/?field-keywords={}",
    "gh":      "https://github.com/search?q={}",
    "yt":      "https://youtube.com/results?search_query={}",

    "def":     "https://merriam-webster.com/dictionary/{}",
    "poe":     "https://pathofexile.gamepedia.com/index.php?search={}",

    "arch":    "https://wiki.archlinux.org/index.php?search={}"
}

c.bindings.commands["normal"] = {
    "T": "set-cmd-text -s :buffer",

    "gt": "tab-next",
    "gT": "tab-prev"
}
