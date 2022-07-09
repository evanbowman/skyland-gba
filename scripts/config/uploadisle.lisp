
;; Ok, so some explanation of the format, in general:
;;
;; Each island takes up roughly 576 bytes in memory. First, I put a single byte
;; identifier into a buffer, indicating the shape of the island (0 == cube, 1 ==
;; flat, 2 == pillar, 3 == freebuild, 4 == atol). Next, I push the island's
;; blocks into the buffer, where z is the outermost loop, followed by x, and y
;; is the innermost loop. I then compress the data with the heatshrink
;; compression library. Then, I base32-encode the compressed data, so that it
;; doesn't contain any characters incompatible with urls. Finally, I prepend the
;; string below, followed by ?d=. If, for some reason, the webserver goes down
;; in the future or if someone wants to create an alternate webserver, the url
;; below could be reconfigured.

"skylandgame.io/sr"
