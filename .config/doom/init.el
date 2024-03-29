;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

(doom!
       :completion
       (company          ; the ultimate code completion backend
        +auto            ; as-you-type code completion
        +childframe)
       (ivy              ; a search engine for love and life
        +fuzzy           ; enable fuzzy search backend for ivy
       ;+prescient       ; enable filtering and sorting of ivy searches with the prescient package
        +childframe)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
      ;(ligatures
      ; +extra)
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
      ;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
      ;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; universal code folding
       (format +onsave)  ; automated prettiness
      ;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
      ;objed             ; text object editing for the innocent
      ;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired            ; making dired pretty [functional]
        +ranger)         ; bringing the goodness of ranger to dired
       ;+icons)          ; colorful icons for dired-mode
       electric          ; smarter, keyword-based electric-indent
       ;ibuffer          ; interactive buffer management
       ;undo (+tree)     ; persistent, advanced undos
       vc                ; version-control and Emacs, sitting in a tree

       :checkers
       (syntax
        +childframe)
       spell
       ;grammar

       :term
       ;eshell            ; a consistent, cross-platform shell (WIP)
       ;term              ; terminals in Emacs
       ;vterm             ; another terminals in Emacs

       :tools
      ;ansible
      ;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
      ;docker
       editorconfig      ; let someone else argue about tabs vs spaces
      ;ein               ; tame Jupyter notebooks with emacs
       (eval             ; run code, run (also, repls)
        +overlay)        ; show result near the cursor
      ;gist              ; interacting with github gists
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       lsp
      ;macos             ; MacOS-specific commands
       (magit
        +forge)
      ;make              ; run make tasks from Emacs
      ;pass              ; password manager for nerds
       pdf               ; pdf enhancements
      ;prodigy           ; FIXME managing external services & code builders
      ;rgb               ; creating color strings
      ;terraform         ; infrastructure as code
      ;tmux              ; an API for interacting with tmux
      ;upload            ; map local to remote projects via ssh/ftp

       :lang
      ;agda              ; types of types of types of types...
      ;assembly          ; assembly for fun or debugging
      ;(cc +irony +rtags); C/C++/Obj-C madness
      ;clojure           ; java with a lisp
      ;common-lisp       ; if you've seen one lisp, you've seen them all
      ;coq               ; proofs-as-programs
      ;crystal           ; ruby at the speed of c
      ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
      ;erlang            ; an elegant language for a more civilized age
      ;elixir            ; erlang done right
      ;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
      ;ess               ; emacs speaks statistics
      ;fsharp            ; ML stands for Microsoft's Language
       (go +lsp)         ; the hipster dialect
       (haskell +dante)  ; a language that's lazier than I am
      ;hy                ; readability of scheme w/ speed of python
      ;idris             ;
      ;(java +meghanada) ; the poster child for carpal tunnel syndrome
      ;javascript        ; all(hope(abandon(ye(who(enter(here))))))
      ;julia             ; a better, faster MATLAB
      ;kotlin            ; a better, slicker Java(Script)
      ;latex             ; writing papers in Emacs has never been so fun
      ;ledger            ; an accounting system in Emacs
      ;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
      ;nim               ; python + lisp at the speed of c
       (nix +lsp)        ; I hereby declare "nix geht mehr!"
      ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +gnuplot
        +noter
        +pandoc
        +present         ; Emacs for presentations
        ; +pretty        ; nice looking bullets and stuff
        +roam2)           ; org-roam
      ;perl              ; write code no one else can comprehend
      ;php               ; perl's insecure younger brother
      ;plantuml          ; diagrams for confusing people more
      ;purescript        ; javascript, but functional
      ;python            ; beautiful is better than ugly
      ;qt                ; the 'cutest' gui framework ever
      ;racket            ; a DSL for DSLs
       rest              ; Emacs as a REST client
      ;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
      ;scala             ; java, but good
       scheme
       sh                ; she sells (ba|z)sh shells on the C xor
      ;solidity          ; do you need a blockchain? No.
      ;swift             ; who asked for emoji variables?
      ;terra             ; Earth and Moon in alignment for performance.
      ;web               ; the tubes
      ;yaml

       :email
       ;(mu4e +gmail)       ; WIP
       ;notmuch             ; WIP
       ;(wanderlust +gmail) ; WIP

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;calendar
      ;irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       literate

       ;; The default module set reasonable defaults for Emacs. It also provides
       ;; a Spacemacs-inspired keybinding scheme. Use it as a reference for your
       ;; own modules.
       (default +bindings +smartparens))
