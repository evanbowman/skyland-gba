
@{{BLOCK(overlay_dialog_fullscreen)

@=======================================================================
@
@	overlay_dialog_fullscreen, 1200x8@4, 
@	Transparent color : FF,00,FF
@	+ palette 16 entries, not compressed
@	+ 150 tiles lz77 compressed
@	Total size: 32 + 772 = 804
@
@	Time-stamp: 2024-07-03, 06:01:43
@	Exported by Cearn's GBA Image Transmogrifier, v0.9.2
@	( http://www.coranac.com/projects/#grit )
@
@=======================================================================

	.section .rodata
	.align	2
	.global overlay_dialog_fullscreenTiles		@ 772 unsigned chars
	.hidden overlay_dialog_fullscreenTiles
overlay_dialog_fullscreenTiles:
	.word 0x0012C010,0xF000003F,0xF001F001,0xF001F001,0xFF01F001,0x01F001F0,0x01F001F0,0x01F001F0
	.word 0x01F001F0,0xF001F0FF,0xF001F001,0xF001F001,0xF001F001,0x01F0FF01,0x01F001F0,0x01F001F0
	.word 0x01F001F0,0xF0FF01F0,0xF001F001,0xF001F001,0xF001F001,0xFF01F001,0x01F001F0,0x01F001F0
	.word 0x01F001F0,0x01F001F0,0xF001F0FF,0xF001F001,0xF001F001,0xF001F001,0x01F0FF01,0x01F001F0
	.word 0x01F001F0,0x01F001F0,0xF0FF01F0,0xF001F001,0xF001F001,0xF001F001,0xFF01F001,0x01F001F0
	.word 0x01F001F0,0x01F001F0,0x01F001F0,0xF001F0FF,0xF001F001,0xF001F001,0xF001F001,0x01F0FF01
	.word 0x01F001F0,0x01F001F0,0x01F001F0,0xF0FF01F0,0xF001F001,0xF001F001,0xF001F001,0xFF01F001
	.word 0x01F001F0,0x01F001F0,0x01F001F0,0x01F001F0,0xF001F0FF,0xF001F001,0xF001F001,0xF001F001

	.word 0x01F0FF01,0x01F001F0,0x01F001F0,0x01F001F0,0xF0FF01F0,0xF001F001,0xF001F001,0xF001F001
	.word 0xFF01F001,0x01F001F0,0x01F001F0,0x01F001F0,0x01F001F0,0xD001F0C7,0x44444501,0x01F001F0
	.word 0x40C001F0,0x6051700B,0x66000066,0x06008766,0x46660044,0x1B000300,0x409F0310,0x30666672
	.word 0xB042F001,0x50275003,0x1100D2A9,0x00640330,0x10066607,0x00FF0061,0x10C9E006,0x10351013
	.word 0x502B2023,0xFF03F037,0xA76007A0,0x03C003F0,0xDB10D300,0xEB108120,0x709F50C0,0x222224C3
	.word 0x37224422,0x08004222,0xA042D800,0x301BF0DC,0x555500FB,0x54455555,0x00814444,0x55555403
	.word 0x00454544,0x44450102,0x54454554,0xBF081055,0xF1551F20,0x403F7026,0xF0C71103,0xF22FF003
	.word 0x5E303FE0,0x1FA062F0,0x97005445,0x00546B54,0x540400AD,0x00540C00,0xFFB5009E,0x33F01200

	.word 0xA630A2F0,0x03E02B10,0x0B30B3F0,0xF094F0FF,0x110E1001,0x112B2123,0x213E0133,0x4B01FF43
	.word 0x3AF15311,0x1FF094C0,0xB3F001F0,0xF0EF0B50,0x003AF1A2,0x2B21541F,0x3B113321,0x11EEF101
	.word 0x10B5204B,0x1C12247F,0x33122B02,0x1F42FF24,0x51400F20,0x4E124612,0x3F023A02,0x72FF0740
	.word 0xF009F35E,0xF001F001,0xF001F001,0xFF01F001,0x01F001F0,0x01F001F0,0x01F001F0,0x01F001F0
	.word 0xF001F0FF,0xF001F001,0xF001F001,0xF001F001,0x01F0FF01,0x01F001F0,0x01F001F0,0x01F001F0
	.word 0xF0FF01F0,0xF001F001,0xF001F001,0xF001F001,0xFF01F001,0x01F001F0,0x01F001F0,0x01F001F0
	.word 0x01F001F0,0xF001F0FF,0xF001F001,0xF001F001,0xF001F001,0x01F0FF01,0x01F001F0,0x01F001F0
	.word 0x01F001F0,0x1100DF66,0x31144441,0x7F144411,0x10037013,0xF07FF413,0xF001F001,0x8001F001

	.word 0x01000170

	.section .rodata
	.align	2
	.global overlay_dialog_fullscreenPal		@ 32 unsigned chars
	.hidden overlay_dialog_fullscreenPal
overlay_dialog_fullscreenPal:
	.hword 0x7C1F,0x42D9,0x2C7D,0x25D2,0x0800,0x737B,0x1FB9,0x0000
	.hword 0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000

@}}BLOCK(overlay_dialog_fullscreen)
