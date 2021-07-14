
//{{BLOCK(overlay)

//======================================================================
//
//	overlay, 8000x8@4, 
//	Transparent color : FF,00,FF
//	+ palette 256 entries, not compressed
//	+ 1000 tiles not compressed
//	Total size: 512 + 32000 = 32512
//
//	Time-stamp: 2021-07-14, 13:59:06
//	Exported by Cearn's GBA Image Transmogrifier, v0.8.16
//	( http://www.coranac.com/projects/#grit )
//
//======================================================================

#ifndef GRIT_OVERLAY_H
#define GRIT_OVERLAY_H

#define overlayTilesLen 32000
extern const unsigned int overlayTiles[8000];

#define overlayPalLen 512
extern const unsigned short overlayPal[256];

#endif // GRIT_OVERLAY_H

//}}BLOCK(overlay)
