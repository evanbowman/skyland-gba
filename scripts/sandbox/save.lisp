
(list
 (cons 'save-protocol 3)
 (cons 'rooms (cons (rooms (player)) (rooms (opponent))))
 (cons 'chrs (cons (chrs (player)) (chrs (opponent))))
 (cons 'terrain (cons (terrain (player)) (terrain (opponent))))
 (cons 'chr-names chr-names))