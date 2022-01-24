(ns sted.sys.kbd)

(def default-keymap
  {"f"   :flow-right
   "S-^" :new-meta
   "S-F" :flow-right-coll
   "u"   :undo
   "S-A" :alias
   "g"   :gobble
   "S-I" :zp
   "o"   :offer
   ";"   :new-comment
   "S-S" :split
   "M-s" :splice
   "S-Z" :drag-left
   "S-X" :drag-right
   "n"   :find-next
   "S-N" :find-first
   "C-/" :undo
   "S-R" :reify-undo
   "S-_" :uneval
   "S-W" :save
   "t"   :tear
   "S-@" :new-deref
   "a"   :flow-left
   "w"   :float
   "s"   :sink
   ;; "S-H"       :toplevel
   "h"         :parent
   "j"         :next
   "k"         :prev
   "l"         :tail
   "r"         :raise
   " "         :insert-right
   "S- "       :insert-left
   "d"         :delete-right
   "S-H"       :hoist
   "Backspace" :delete-left
   ;; "Backspace" :move-to-deleted-chain
   "Enter"     :linebreak
   "C-Enter"   :insert-right-newline
   "Escape"    :select-chain
   "c"         :clone
   "z"         :hop-left
   "x"         :hop-right
   "9"         :wrap
   ;; "9"         :new-list
   "0"     :parent
   "]"     :parent
   "p"     :slurp-right
   "S-P"   :barf-right
   "Tab"   :indent
   "S-Tab" :dedent
   "e"     :eval-sci
   "S-("   :new-list
   "["     :new-vec
   "S-C"   :new-chain
   "S-B"   :new-bar "'" :new-quote
   "1"     :m1
   "2"     :m2
   "3"     :m3
   "4"     :m4
   "5"     :m5
   "6"     :m6
   "7"     :m7
   "8"     :m8
   "v"     :scroll
   "-"     :hide
   "i"     :insert-left
   "S-Q"   :stringify
   "S-+"   :plus})

#?(:cljs
   (defn event->kbd
     [^KeyboardEvent ev]
     (str (when (.-altKey ev) "M-")
          (when (.-ctrlKey ev)
            "C-")
          (when (.-shiftKey ev) "S-")
          (.-key ev))))

(defn kbd->keydowns
  [kbd]
  (let [[[_m mod1 mod2 mod3 root]] (re-seq #"(.-)?(.-)?(.-)?(.*)" kbd) 
        mods (set [mod1 mod2 mod3])]
    (cond->> [root]
      (contains? mods "S-") (into ["ShiftLeft"])
      (contains? mods "M-") (into ["AltLeft"])
      (contains? mods "C-") (into ["Control"]))))

