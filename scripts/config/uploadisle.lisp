
;; Ok, so some explanation of the format, in general: For the highscore upload
;; format, I did not bother to describe anything at all, because I don't want
;; people to cheat and upload erroneous highscore values. But I would like to
;; explain the island upload format, for posterity...
;;
;; Each island takes up roughly 576 bytes in memory. First, I put a single byte
;; identifier into a buffer, indicating the shape of the island (0 == cube, 1 ==
;; flat, 2 == pillar, 3 == freebuild, 4 == atol). Next, I push the island's
;; blocks into the buffer, where z is the outermost loop, followed by x, and y
;; is the innermost loop. I then compress the data with the heatshrink
;; compression library. Then, I base32-encode the compressed data, so that it
;; doesn't contain any characters incompatible with urls. Finally, I prepend the
;; string below, followed by ?d=.

"skylandgame.io/sr"
