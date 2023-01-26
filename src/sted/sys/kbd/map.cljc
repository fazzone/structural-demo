(ns sted.sys.kbd.map)

(def default
  {" "         :insert-right
   "-"         :hide
   "."         :save-demo
   "/"         :search/start
   "0"         :parent
   "1"         :m1
   "2"         :m2
   "3"         :m3
   "4"         :m4
   "5"         :m5
   "6"         :m6
   "7"         :m7
   "8"         :m8
   "9"         :wrap
   ";"         :new-comment
   "Backspace" :move-to-deleted-chain
   "C-/"       :undo
   "C-Enter"   :insert-right-newline
   "C-s"       :search/start
   "Enter"     :linebreak
   "Escape"    :select-chain
   "M-S-I"     "No, I do not want to report a problem to Google"
   "M-S-O"     :oneline-all
   "M-c"       :clone-parent
   "M-l"       :clear-one-eval
   "M-r"       :raise-parent
   "M-s"       :splice
   "S- "       :insert-left
   "S-("       :new-list
   "S-+"       :plus
   "S-@"       :new-deref
   "S-A"       :alias
   "S-B"       :new-bar "'" :new-quote
   "S-C"       :new-chain
   "S-F"       :flow-right-coll
   "S-H"       :hoist
   "S-I"       :zp
   "S-M"       :multiline
   "S-N"       :find-first
   "S-O"       :oneline
   "S-P"       :barf-right
   "S-Q"       :stringify
   "S-R"       :unraise
   "S-S"       :split
   "S-Tab"     "Nothing yet"
   "S-W"       :save
   "S-X"       :drag-right
   "S-Z"       :drag-left
   "S-^"       :new-meta
   "S-_"       :uneval
   "Tab"       :zp
   "["         :new-vec
   "]"         :parent
   "a"         :flow-left
   "c"         :clone
   "d"         :delete-right
   "e"         :eval-sci
   "f"         :flow-right
   "g"         :gobble
   "h"         :parent
   "i"         :insert-left
   "j"         :prev
   "k"         :next
   "l"         :tail
   "n"         :find-next
   "o"         :offer
   "p"         :slurp-right
   "q"         :compose
   "r"         :raise
   "s"         :sink
   "t"         :tear
   "u"         :undo
   "v"         :scroll
   "w"         :float
   "x"         :hop-right
   "z"         :hop-left
   ;; "9"         :new-list
   ;; "Backspace" :delete-left
   ;; "S-H"       :toplevel
   ;; "S-R" :reify-undo
   ;; "S-Tab"     :dedent
   ;; "j"   :next
   ;; "k"   :prev
   ;; lispm
   ;; vi
   })

(defn kbd->keydowns
  [kbd]
  (let [[[_m mod1 mod2 mod3 root]] (re-seq #"(.-)?(.-)?(.-)?(.*)" kbd) 
        mods (set [mod1 mod2 mod3])]
    (cond->> [root]
      (contains? mods "S-") (into ["ShiftLeft"])
      (contains? mods "M-") (into ["AltLeft"])
      (contains? mods "C-") (into ["Control"]))))



