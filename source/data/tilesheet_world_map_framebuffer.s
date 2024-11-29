
@{{BLOCK(tilesheet_world_map_framebuffer)

@=======================================================================
@
@	tilesheet_world_map_framebuffer, 16x16@4, 
@	Transparent color : FF,00,FF
@	+ palette 16 entries, not compressed
@	+ 4 tiles Metatiled by 2x2 lz77 compressed
@	Total size: 32 + 24 = 56
@
@	Time-stamp: 2024-11-29, 15:36:54
@	Exported by Cearn's GBA Image Transmogrifier, v0.9.2
@	( http://www.coranac.com/projects/#grit )
@
@=======================================================================

	.section .rodata
	.align	2
	.global tilesheet_world_map_framebufferTiles		@ 24 unsigned chars
	.hidden tilesheet_world_map_framebufferTiles
tilesheet_world_map_framebufferTiles:
	.word 0x00008010,0xF000003F,0xF001F001,0xF001F001,0x8001F001,0x000001F0

	.section .rodata
	.align	2
	.global tilesheet_world_map_framebufferPal		@ 32 unsigned chars
	.hidden tilesheet_world_map_framebufferPal
tilesheet_world_map_framebufferPal:
	.hword 0x0000,0x5FE9,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000
	.hword 0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421,0x0421

@}}BLOCK(tilesheet_world_map_framebuffer)
