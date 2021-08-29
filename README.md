#!/usr/bin/env bash
# Setup
[[ $1 = "first-time-setup" ]] && npm install
# Development server
[[ $1 = "go" ]] && clj && echo "Open localhost:8087"
# Compile cljs with optimizations
[[ $1 = "compile-cljs" ]] && clj -M -e "(user/release-cljs-and-exit)"
# Publish to github pages
[[ $1 = "publish-ghpages" ]] && $0 compile-cljs && git add srv/js/main.js && git commit -m "publish" && git push origin
