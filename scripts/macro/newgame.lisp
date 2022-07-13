

(eval-file "/scripts/reset_hooks.lisp")


(coins-set 200)

(eval-file "/scripts/macro/start_layout.lisp")

(dialog "<f:70>Welcome to the macrocosm! You and a group of settlers have just established a colony on a remote isle. Provide your population with food and housing, and it'll grow!")

(defn on-dialog-closed
  (dialog "Produce commodities for extra coins. Commodities are cheaper to build near blocks improved with the same commodity, but produce diminishing returns. Export commodities to other islands to balance demand and scarcity.")

  (defn on-dialog-closed
    (dialog "But first! Make sure to plant some grains, your citizens will need food!")

    (defn on-dialog-closed
      (syscall "fade" 0))))
