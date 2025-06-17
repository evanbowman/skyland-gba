;;;
;;; The game invokes this script when creating a save file. The result of this
;;; script will be stored in save memory, and passed to restore_save.lisp when
;;; reloading a game. As you see below, the list itself is an association list,
;;; and you may insert new key-value pairs if you have any data in particular
;;; that you'd like to save.
;;;


(list
 (cons 'save-protocol 4)
 (cons 'rooms (rooms (player)))
 (cons 'chrs (chrs (player)))
 (cons 'enemies-seen enemies-seen)
 (cons 'friendlies-seen friendlies-seen)
 (cons 'surprises-seen surprises-seen)
 (cons 'last-zone last-zone)
 (cons 'quests quests)
 (cons 'terrain (terrain (player)))
 ;; NOTE: I changed the function name from diff to difficulty, but need to
 ;; preserve the old symbol for backwards compatibility with people's save
 ;; files.
 (cons 'diff (difficulty))
 (cons 'qids qids)
 (cons 'qvar qvar)
 (cons 'adventure-log adventure-log)
 (cons 'zone-shop-items zone-shop-items)
 (cons 'groups (groups))
 (cons 'nav (wg-nav-path))
 (cons 'faction (faction))
 (cons 'pending-events pending-events))
