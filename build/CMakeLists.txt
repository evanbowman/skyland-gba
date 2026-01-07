cmake_minimum_required(VERSION 3.5)

# Oh cmake... what a mess.


project(SkylandEngine)

set(ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)
set(SOURCE_DIR ${ROOT_DIR}/source)
set(IMAGE_DIR ${ROOT_DIR}/images)


set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
enable_language(ASM)



option(PSP "PlayStationPortable" OFF)
option(GAMEBOY_ADVANCE "GameboyAdvance" ON)
option(NINTENDO_DS "NintendoDS" OFF)
option(GBA_AUTOBUILD_IMG "AutobuildImg" OFF)


if(GAMEBOY_ADVANCE AND NOT DEVKITARM)
  message(WARNING "Note: GAMEBOY_ADVANCE option is ON by default.")
  message(FATAL_ERROR "GAMEBOY_ADVANCE option is set, but missing ARM toolchain.")
elseif(NINTENDO_DS AND NOT DEVKITARM)
  message(WARNING "Note: NINTENDO_DS option is ON by default.")
  message(FATAL_ERROR "NINTENDO_DS option is set, but missing ARM toolchain.")
endif()


if(GAMEBOY_ADVANCE AND WIN32)
  message(FATAL_ERROR "Gameboy Advance builds not supported in windows")
endif()


if(GAMEBOY_ADVANCE AND NINTENDO_DS)
  message(FATAL_ERROR
    "Cannot build Gameboy Advance and NintendoDS roms with a single toolchain!")
endif()


include_directories(${SOURCE_DIR})
include_directories(${ROOT_DIR}/external/)


if(APPLE)
  set(EMACS_FLYCHECK_FRONTEND clang)
else()
  set(EMACS_FLYCHECK_FRONTEND gcc)
endif()

# For emacs flycheck users
configure_file(dirlocals.in ${SOURCE_DIR}/.dir-locals.el)



set(G_COMPILE_FLAGS
  -g3
  -Wall
  -Werror
  -pedantic
  -ffast-math
  -fno-math-errno
  # -fno-exceptions inexplicably breaks some code
  -Wdouble-promotion
  -fomit-frame-pointer)



set(SOURCES
  ${SOURCE_DIR}/platform/flash_filesystem.cpp
  ${SOURCE_DIR}/platform/ram_filesystem.cpp
  ${SOURCE_DIR}/platform/scratch_buffer.cpp
  ${SOURCE_DIR}/platform/conf.cpp
  ${SOURCE_DIR}/graphics/overlay.cpp
  ${SOURCE_DIR}/graphics/sprite.cpp
  ${SOURCE_DIR}/number/numeric.cpp
  ${SOURCE_DIR}/number/random.cpp
  ${SOURCE_DIR}/graphics/view.cpp
  ${SOURCE_DIR}/localization.cpp
  ${SOURCE_DIR}/localization_extended.cpp
  ${SOURCE_DIR}/memory/malloc.cpp
  ${SOURCE_DIR}/memory/extension.cpp
  ${SOURCE_DIR}/compression.cpp
  ${SOURCE_DIR}/memory/pool.cpp
  ${SOURCE_DIR}/collision.cpp
  ${SOURCE_DIR}/globals.cpp
  ${SOURCE_DIR}/base32.cpp
  ${SOURCE_DIR}/string.cpp
  ${SOURCE_DIR}/start.cpp
  ${SOURCE_DIR}/rle.cpp
  ${SOURCE_DIR}/qr.cpp)


set(SOURCES
  ${SOURCES}
  ${SOURCE_DIR}/skyland/flag.cpp
  ${SOURCE_DIR}/skyland/path.cpp
  ${SOURCE_DIR}/skyland/save.cpp
  ${SOURCE_DIR}/skyland/paint.cpp
  ${SOURCE_DIR}/skyland/sound.cpp
  ${SOURCE_DIR}/skyland/camera.cpp
  ${SOURCE_DIR}/skyland/console.cpp
  ${SOURCE_DIR}/skyland/skyland.cpp
  ${SOURCE_DIR}/skyland/network.cpp
  ${SOURCE_DIR}/skyland/dataCart.cpp
  ${SOURCE_DIR}/skyland/roomPool.cpp
  ${SOURCE_DIR}/skyland/bulkTimer.cpp
  ${SOURCE_DIR}/skyland/loginToken.cpp
  ${SOURCE_DIR}/skyland/script_defs.cpp
  ${SOURCE_DIR}/skyland/achievement.cpp
  ${SOURCE_DIR}/skyland/systemString.cpp
  ${SOURCE_DIR}/skyland/configure_island.cpp
  ${SOURCE_DIR}/skyland/player/playerP1.cpp
  ${SOURCE_DIR}/skyland/player/sandboxSpectatorPlayer.cpp
  ${SOURCE_DIR}/skyland/player/coOpTeam.cpp
  ${SOURCE_DIR}/skyland/player/autopilotPlayer.cpp
  ${SOURCE_DIR}/skyland/player/macroFreebuildTeam.cpp
  ${SOURCE_DIR}/skyland/player/opponent/opponent.cpp
  ${SOURCE_DIR}/skyland/player/opponent/enemyAI.cpp
  ${SOURCE_DIR}/skyland/player/opponent/friendlyAI.cpp
  ${SOURCE_DIR}/skyland/player/opponent/procgenEnemyAI.cpp
  ${SOURCE_DIR}/skyland/player/opponent/multiplayerPeer.cpp
  ${SOURCE_DIR}/skyland/weather/night.cpp
  ${SOURCE_DIR}/skyland/weather/storm.cpp
  ${SOURCE_DIR}/skyland/weather/typhoon.cpp
  ${SOURCE_DIR}/skyland/weather/dynamic.cpp
  ${SOURCE_DIR}/skyland/weather/dustStorm.cpp
  ${SOURCE_DIR}/skyland/weather/blizzard.cpp
  ${SOURCE_DIR}/skyland/weather/solarStorm.cpp
  ${SOURCE_DIR}/skyland/weather/clearSkies.cpp
  ${SOURCE_DIR}/skyland/weather/environment.cpp
  ${SOURCE_DIR}/skyland/weather/slightlyOvercast.cpp
  ${SOURCE_DIR}/skyland/scene.cpp
  ${SOURCE_DIR}/skyland/scene_pool.cpp
  ${SOURCE_DIR}/skyland/sharedVariable.cpp
  ${SOURCE_DIR}/skyland/room_metatable.cpp
  ${SOURCE_DIR}/skyland/timeStream.cpp
  ${SOURCE_DIR}/skyland/entity.cpp
  ${SOURCE_DIR}/skyland/entity/explosion/explosion.cpp
  ${SOURCE_DIR}/skyland/entity/explosion/exploTrail.cpp
  ${SOURCE_DIR}/skyland/entity/explosion/exploSpawner.cpp
  ${SOURCE_DIR}/skyland/entity/character/character.cpp
  ${SOURCE_DIR}/skyland/entity/drones/drone.cpp
  ${SOURCE_DIR}/skyland/entity/drones/reconstructionDrone.cpp
  ${SOURCE_DIR}/skyland/entity/drones/droneMeta.cpp
  ${SOURCE_DIR}/skyland/entity/birds/genericBird.cpp
  ${SOURCE_DIR}/skyland/entity/macro/macrocosmEntity.cpp
  ${SOURCE_DIR}/skyland/entity/misc/smokePuff.cpp
  ${SOURCE_DIR}/skyland/scene/module.cpp
  ${SOURCE_DIR}/skyland/scene/desktopOS.cpp
  ${SOURCE_DIR}/skyland/scene/hintScene.cpp
  ${SOURCE_DIR}/skyland/scene/worldScene.cpp
  ${SOURCE_DIR}/skyland/scene/readyScene.cpp
  ${SOURCE_DIR}/skyland/scene/fadeInScene.cpp
  ${SOURCE_DIR}/skyland/scene/rewindScene.cpp
  ${SOURCE_DIR}/skyland/scene/debriefScene.cpp
  ${SOURCE_DIR}/skyland/scene/fadeOutScene.cpp
  ${SOURCE_DIR}/skyland/scene/itemShopScene.cpp
  ${SOURCE_DIR}/skyland/scene/qrViewerScene.cpp
  ${SOURCE_DIR}/skyland/scene/lispReplScene.cpp
  ${SOURCE_DIR}/skyland/scene/selInputScene.cpp
  ${SOURCE_DIR}/skyland/scene/moveRoomScene.cpp
  ${SOURCE_DIR}/skyland/scene/worldMapScene.cpp
  ${SOURCE_DIR}/skyland/scene/hideRoomsScene.cpp
  ${SOURCE_DIR}/skyland/scene/keyComboScene.cpp
  ${SOURCE_DIR}/skyland/scene/crewStatsScene.cpp
  ${SOURCE_DIR}/skyland/scene/textEntryScene.cpp
  ${SOURCE_DIR}/skyland/scene/inspectP2Scene.cpp
  ${SOURCE_DIR}/skyland/scene/loadLevelScene.cpp
  ${SOURCE_DIR}/skyland/scene/zoneImageScene.cpp
  ${SOURCE_DIR}/skyland/scene/startMenuScene.cpp
  ${SOURCE_DIR}/skyland/scene/selectMenuScene.cpp
  ${SOURCE_DIR}/skyland/scene/fatalErrorScene.cpp
  ${SOURCE_DIR}/skyland/scene/placeDroneScene.cpp
  ${SOURCE_DIR}/skyland/scene/scriptHookScene.cpp
  ${SOURCE_DIR}/skyland/scene/createFileScene.cpp
  ${SOURCE_DIR}/skyland/scene/boxedDialogScene.cpp
  ${SOURCE_DIR}/skyland/scene/salvageRoomScene.cpp
  ${SOURCE_DIR}/skyland/scene/titleScreenScene.cpp
  ${SOURCE_DIR}/skyland/scene/sandboxResetScene.cpp
  ${SOURCE_DIR}/skyland/scene/scriptedMenuScene.cpp
  ${SOURCE_DIR}/skyland/scene/introCreditsScene.cpp
  ${SOURCE_DIR}/skyland/scene/composeSynthScene.cpp
  ${SOURCE_DIR}/skyland/scene/salvageDroneScene.cpp
  ${SOURCE_DIR}/skyland/scene/constructionScene.cpp
  ${SOURCE_DIR}/skyland/scene/setGamespeedScene.cpp
  ${SOURCE_DIR}/skyland/scene/notificationScene.cpp
  ${SOURCE_DIR}/skyland/scene/selectSampleScene.cpp
  ${SOURCE_DIR}/skyland/scene/adventureLogScene.cpp
  ${SOURCE_DIR}/skyland/scene/moveCharacterScene.cpp
  ${SOURCE_DIR}/skyland/scene/upgradePromptScene.cpp
  ${SOURCE_DIR}/skyland/scene/startAdventureScene.cpp
  ${SOURCE_DIR}/skyland/scene/constructDroneScene.cpp
  ${SOURCE_DIR}/skyland/scene/groupSelectionScene.cpp
  ${SOURCE_DIR}/skyland/scene/selectChallengeScene.cpp
  ${SOURCE_DIR}/skyland/scene/modifierKeyHintScene.cpp
  ${SOURCE_DIR}/skyland/scene/weaponSetTargetScene.cpp
  ${SOURCE_DIR}/skyland/scene/fullscreenDialogScene.cpp
  ${SOURCE_DIR}/skyland/scene/recoverCharacterScene.cpp
  ${SOURCE_DIR}/skyland/scene/repairDroneRangeScene.cpp
  ${SOURCE_DIR}/skyland/scene/assignWeaponGroupScene.cpp
  ${SOURCE_DIR}/skyland/scene/sramFileWritebackScene.cpp
  ${SOURCE_DIR}/skyland/scene/transportCharacterScene.cpp
  ${SOURCE_DIR}/skyland/scene/multiplayerConnectScene.cpp
  ${SOURCE_DIR}/skyland/scene/multiplayerSettingsScene.cpp
  ${SOURCE_DIR}/skyland/scene/replicatorSelectionScene.cpp
  ${SOURCE_DIR}/skyland/scene/combatDroneSetTargetScene.cpp
  ${SOURCE_DIR}/skyland/scene/levelCompleteOptionsScene.cpp
  ${SOURCE_DIR}/skyland/scene/playerIslandDestroyedScene.cpp
  ${SOURCE_DIR}/skyland/scene/multiplayerCoOpAwaitLockScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/helpScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/macrocosmScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/macroverseScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/nextTurnScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/keyComboScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/selectorScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/tileOptionsScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/createBlockScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/menuOptionsScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/moveCheckerScene.cpp
  ${SOURCE_DIR}/skyland/scene/macro/modifiedSelectorScene.cpp
  ${SOURCE_DIR}/skyland/scene/modules/developerModeModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/regressionModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/factoryResetModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/bugReportModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/feedbackModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/colorProfileModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/creditsModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/checkersModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/macrocosmFreebuildModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/dataCartModule.cpp
  ${SOURCE_DIR}/skyland/scene/highscoresScene.cpp
  ${SOURCE_DIR}/skyland/scene/modules/dlcManagerModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/dlcInjectorModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/fileBrowserModule.cpp
  ${SOURCE_DIR}/skyland/scene/selectTutorialScene.cpp
  ${SOURCE_DIR}/skyland/scene/modules/macrocosmLoaderModule.cpp
  ${SOURCE_DIR}/skyland/scene/achievementNotificationScene.cpp
  ${SOURCE_DIR}/skyland/scene/modules/achievementViewerModule.cpp
  ${SOURCE_DIR}/skyland/scene/adventureModeSettingsScene.cpp
  ${SOURCE_DIR}/skyland/scene/modules/glossaryViewerModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/sandboxLoaderModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/textEditorModule.cpp
  ${SOURCE_DIR}/skyland/scene/modules/skylandForever.cpp
  ${SOURCE_DIR}/skyland/scene/modules/flagDesignerModule.cpp)



# All of these sources are critical to the game code, and should be compiled
# with the highest optimization settings.
add_library(FASTPATH
  ${SOURCE_DIR}/skyland/room.cpp
  ${SOURCE_DIR}/skyland/island.cpp
  ${SOURCE_DIR}/skyland/minimap.cpp
  ${SOURCE_DIR}/skyland/rooms/ice.cpp
  ${SOURCE_DIR}/skyland/rooms/tnt.cpp
  ${SOURCE_DIR}/skyland/rooms/bell.cpp
  ${SOURCE_DIR}/skyland/rooms/core.cpp
  ${SOURCE_DIR}/skyland/rooms/hull.cpp
  ${SOURCE_DIR}/skyland/rooms/lava.cpp
  ${SOURCE_DIR}/skyland/rooms/cloak.cpp
  ${SOURCE_DIR}/skyland/rooms/crane.cpp
  ${SOURCE_DIR}/skyland/rooms/radar.cpp
  ${SOURCE_DIR}/skyland/rooms/synth.cpp
  ${SOURCE_DIR}/skyland/rooms/water.cpp
  ${SOURCE_DIR}/skyland/rooms/arcGun.cpp
  ${SOURCE_DIR}/skyland/rooms/bridge.cpp
  ${SOURCE_DIR}/skyland/rooms/cannon.cpp
  ${SOURCE_DIR}/skyland/rooms/basalt.cpp
  ${SOURCE_DIR}/skyland/rooms/ladder.cpp
  ${SOURCE_DIR}/skyland/rooms/weapon.cpp
  ${SOURCE_DIR}/skyland/rooms/canvas.cpp
  ${SOURCE_DIR}/skyland/rooms/barrier.cpp
  ${SOURCE_DIR}/skyland/rooms/speaker.cpp
  ${SOURCE_DIR}/skyland/rooms/flakGun.cpp
  ${SOURCE_DIR}/skyland/rooms/qrBlock.cpp
  ${SOURCE_DIR}/skyland/rooms/reactor.cpp
  ${SOURCE_DIR}/skyland/rooms/windmill.cpp
  ${SOURCE_DIR}/skyland/rooms/bulkhead.cpp
  ${SOURCE_DIR}/skyland/rooms/nemesis.cpp
  ${SOURCE_DIR}/skyland/rooms/beamGun.cpp
  ${SOURCE_DIR}/skyland/rooms/warhead.cpp
  ${SOURCE_DIR}/skyland/rooms/ballista.cpp
  ${SOURCE_DIR}/skyland/rooms/mycelium.cpp
  ${SOURCE_DIR}/skyland/rooms/radiator.cpp
  ${SOURCE_DIR}/skyland/rooms/workshop.cpp
  ${SOURCE_DIR}/skyland/rooms/droneBay.cpp
  ${SOURCE_DIR}/skyland/rooms/cargoBay.cpp
  ${SOURCE_DIR}/skyland/rooms/chaosCore.cpp
  ${SOURCE_DIR}/skyland/rooms/deflector.cpp
  ${SOURCE_DIR}/skyland/rooms/clumpBomb.cpp
  ${SOURCE_DIR}/skyland/rooms/warEngine.cpp
  ${SOURCE_DIR}/skyland/rooms/infirmary.cpp
  ${SOURCE_DIR}/skyland/rooms/decimator.cpp
  ${SOURCE_DIR}/skyland/rooms/solarCell.cpp
  ${SOURCE_DIR}/skyland/rooms/stairwell.cpp
  ${SOURCE_DIR}/skyland/rooms/ionCannon.cpp
  ${SOURCE_DIR}/skyland/rooms/ionFizzler.cpp
  ${SOURCE_DIR}/skyland/rooms/replicator.cpp
  ${SOURCE_DIR}/skyland/rooms/fireCharge.cpp
  ${SOURCE_DIR}/skyland/rooms/pluginRoom.cpp
  ${SOURCE_DIR}/skyland/rooms/forcefield.cpp
  ${SOURCE_DIR}/skyland/rooms/bronzeHull.cpp
  ${SOURCE_DIR}/skyland/rooms/mirrorHull.cpp
  ${SOURCE_DIR}/skyland/rooms/rocketSilo.cpp
  ${SOURCE_DIR}/skyland/rooms/annihilator.cpp
  ${SOURCE_DIR}/skyland/rooms/incinerator.cpp
  ${SOURCE_DIR}/skyland/rooms/poweredHull.cpp
  ${SOURCE_DIR}/skyland/rooms/stackedHull.cpp
  ${SOURCE_DIR}/skyland/rooms/sparkCannon.cpp
  ${SOURCE_DIR}/skyland/rooms/manufactory.cpp
  ${SOURCE_DIR}/skyland/rooms/missileSilo.cpp
  ${SOURCE_DIR}/skyland/rooms/sylphCannon.cpp
  ${SOURCE_DIR}/skyland/rooms/transporter.cpp
  ${SOURCE_DIR}/skyland/rooms/escapeBeacon.cpp
  ${SOURCE_DIR}/skyland/rooms/phaseShifter.cpp
  ${SOURCE_DIR}/skyland/rooms/commandModule.cpp
  ${SOURCE_DIR}/skyland/rooms/overdriveCore.cpp
  ${SOURCE_DIR}/skyland/rooms/plunderedRoom.cpp
  ${SOURCE_DIR}/skyland/rooms/particleLance.cpp
  ${SOURCE_DIR}/skyland/rooms/resonanceCore.cpp
  ${SOURCE_DIR}/skyland/rooms/weatherEngine.cpp
  ${SOURCE_DIR}/skyland/rooms/targetingComputer.cpp
  # ${SOURCE_DIR}/skyland/rooms/swerveMissileSilo.cpp
  ${SOURCE_DIR}/skyland/macrocosmSector.cpp
  ${SOURCE_DIR}/skyland/macrocosmCubeSector.cpp
  ${SOURCE_DIR}/skyland/macrocosmPillarSector.cpp
  ${SOURCE_DIR}/skyland/macrocosmPancakeSector.cpp
  ${SOURCE_DIR}/skyland/macrocosmEngine.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/beam.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/flak.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/missile.cpp
  # ${SOURCE_DIR}/skyland/entity/projectile/swerveMissile.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/cannonball.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/incineratorBolt.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/projectile.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/antimatter.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/fireBolt.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/nemesisBlast.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/ballistaBolt.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/arcBolt.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/ionBurst.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/pluginProjectile.cpp
  ${SOURCE_DIR}/skyland/entity/projectile/decimatorBurst.cpp)



target_compile_options(FASTPATH PRIVATE
  -O3
  -Wsuggest-override
  ${G_COMPILE_FLAGS})




set(LISP_SOURCES
  ${SOURCE_DIR}/script/lisp.cpp
  ${SOURCE_DIR}/script/compiler.cpp
  ${SOURCE_DIR}/script/vm.cpp)


add_library(LISP ${LISP_SOURCES})


target_compile_options(LISP PRIVATE
  -g
  -O3 # Script execution is a bit of a bottleneck
  -fno-rtti
  -Wsuggest-override
  ${G_COMPILE_FLAGS})



if(NINTENDO_DS OR GAMEBOY_ADVANCE)

  set(DATA_DIR ${SOURCE_DIR}/data/)


  # NOTE: The nintendo ds uses pretty much the same image format as GBA. I
  # didn't bother to change the asset build pipeline when porting the game.
  set(SOURCES
    ${SOURCES}
    # Data files below. Because the gameboy advance does not have a filesystem,
    # all of the sounds and images need to be compiled into the ROM.
    ${DATA_DIR}/overlay.s
    ${DATA_DIR}/overlay_qr.s
    ${DATA_DIR}/overlay_dialog.s
    ${DATA_DIR}/appendix.s
    ${DATA_DIR}/overlay_datacart.s
    ${DATA_DIR}/overlay_textview.s
    ${DATA_DIR}/overlay_dialog_fullscreen.s
    ${DATA_DIR}/overlay_world_map.s
    ${DATA_DIR}/overlay_adventurelog.s
    ${DATA_DIR}/colorProfiles.s
    ${DATA_DIR}/stack_overflow_flattened.s
    ${DATA_DIR}/traded_core_flattened.s
    ${DATA_DIR}/photo_village_flattened.s
    ${DATA_DIR}/photo_surface_flattened.s
    ${DATA_DIR}/photo_market_flattened.s
    ${DATA_DIR}/photo_disaster_flattened.s
    ${DATA_DIR}/photo_incinerator_flattened.s
    ${DATA_DIR}/photo_goblin_stronghold_flattened.s
    ${DATA_DIR}/tilesheet_world_map_backdrop.s
    ${DATA_DIR}/tilesheet_world_map_framebuffer.s
    ${DATA_DIR}/overlay_large_numeral.s
    ${DATA_DIR}/overlay_island_destroyed.s
    ${DATA_DIR}/overlay_challenges.s
    ${DATA_DIR}/overlay_gamespeed.s
    ${DATA_DIR}/overlay_editor.s
    ${DATA_DIR}/overlay_skyland_title.s
    ${DATA_DIR}/overlay_os.s
    ${DATA_DIR}/wallpaper_flattened.s
    ${DATA_DIR}/hint_infirmary.s
    ${DATA_DIR}/hint_goblin.s
    ${DATA_DIR}/hint_plunder.s
    ${DATA_DIR}/hint_damaged_core.s
    ${DATA_DIR}/charset.s
    ${DATA_DIR}/charset_extended_0.s
    ${DATA_DIR}/charset_extended_1.s
    ${DATA_DIR}/tilesheet.s
    ${DATA_DIR}/tilesheet_enemy_0.s
    ${DATA_DIR}/tilesheet_enemy_0_interior.s
    ${DATA_DIR}/tilesheet_interior.s
    ${DATA_DIR}/tilesheet_top.s
    ${DATA_DIR}/skyland_title_5_flattened.s
    ${DATA_DIR}/skyland_title_4_flattened.s
    ${DATA_DIR}/skyland_title_3_flattened.s
    ${DATA_DIR}/skyland_title_2_flattened.s
    ${DATA_DIR}/skyland_title_1_flattened.s
    ${DATA_DIR}/skyland_title_0_flattened.s
    ${DATA_DIR}/flags.s
    ${DATA_DIR}/character_art.s
    ${DATA_DIR}/paint_icons.s
    ${DATA_DIR}/boot_img_flattened.s
    ${DATA_DIR}/savegame_flattened.s
    ${DATA_DIR}/goblin_savegame_flattened.s
    ${DATA_DIR}/sylph_savegame_flattened.s
    ${DATA_DIR}/ending_img_flattened.s
    ${DATA_DIR}/macro_rendertexture.s
    ${DATA_DIR}/developer_mode_flattened.s
    ${DATA_DIR}/gba_health_safety_logo_flattened.s
    ${DATA_DIR}/intro_crops_snow_flattened.s
    ${DATA_DIR}/intro_crops_snow_far_flattened.s
    ${DATA_DIR}/intro_island_launch_flattened.s
    ${DATA_DIR}/intro_explosion_1_flattened.s
    ${DATA_DIR}/intro_explosion_2_flattened.s
    ${DATA_DIR}/intro_storm_king_flattened.s
    ${DATA_DIR}/zone_image_0_flattened.s
    ${DATA_DIR}/zone_image_1_flattened.s
    ${DATA_DIR}/zone_image_2_flattened.s
    ${DATA_DIR}/zone_image_3_flattened.s
    ${DATA_DIR}/zone_image_4_flattened.s
    ${DATA_DIR}/datacarts_flattened.s
    ${DATA_DIR}/glossary_wall_cg_flattened.s
    ${DATA_DIR}/glossary_weapon_cg_flattened.s
    ${DATA_DIR}/glossary_factory_cg_flattened.s
    ${DATA_DIR}/glossary_power_cg_flattened.s
    ${DATA_DIR}/glossary_misc_cg_flattened.s
    ${DATA_DIR}/glossary_decoration_cg_flattened.s
    ${DATA_DIR}/spritesheet.s
    ${DATA_DIR}/background.s
    ${DATA_DIR}/background_macro.s
    ${DATA_DIR}/background_macro_night.s
    ${DATA_DIR}/background_title_screen.s
    ${DATA_DIR}/spritesheet_title_screen.s
    ${DATA_DIR}/spritesheet_intro.s
    ${DATA_DIR}/spritesheet_macro.s
    ${DATA_DIR}/spritesheet_macroverse.s
    ${DATA_DIR}/spritesheet_os.s
    ${DATA_DIR}/music_unaccompanied_wind.s
    ${DATA_DIR}/music_struttin.s
    ${DATA_DIR}/sound_archivist.s
    ${DATA_DIR}/sound_tonal_flutter.s
    ${DATA_DIR}/sound_powerdown.s
    ${DATA_DIR}/sound_poweron.s
    ${DATA_DIR}/sound_door.s
    ${DATA_DIR}/sound_cancel.s
    ${DATA_DIR}/sound_page_flip.s
    ${DATA_DIR}/sound_seagull_1.s
    ${DATA_DIR}/sound_seagull_2.s
    ${DATA_DIR}/sound_creaking.s
    ${DATA_DIR}/sound_ion_cannon.s
    ${DATA_DIR}/sound_pong_blip1.s
    ${DATA_DIR}/sound_pong_blip2.s
    ${DATA_DIR}/sound_msg.s
    ${DATA_DIR}/sound_pop.s
    ${DATA_DIR}/sound_coin.s
    ${DATA_DIR}/sound_bell.s
    ${DATA_DIR}/sound_thud.s
    ${DATA_DIR}/sound_click.s
    ${DATA_DIR}/sound_cling.s
    ${DATA_DIR}/sound_heart.s
    ${DATA_DIR}/sound_dodge.s
    ${DATA_DIR}/sound_creak.s
    ${DATA_DIR}/sound_build0.s
    ${DATA_DIR}/sound_cannon.s
    ${DATA_DIR}/sound_insert_cart.s
    ${DATA_DIR}/sound_missile.s
    ${DATA_DIR}/sound_thunder_1.s
    ${DATA_DIR}/sound_thunder_2.s
    ${DATA_DIR}/sound_thunder_close_1.s
    ${DATA_DIR}/sound_glass_break.s
    ${DATA_DIR}/sound_missile_explosion.s
    ${DATA_DIR}/sound_weapon_target.s
    ${DATA_DIR}/sound_transporter.s
    ${DATA_DIR}/sound_gust.s
    ${DATA_DIR}/sound_gust2.s
    ${DATA_DIR}/sound_click_negative.s
    ${DATA_DIR}/sound_click_wooden.s
    ${DATA_DIR}/sound_cursor_click.s
    ${DATA_DIR}/sound_button_wooden.s
    ${DATA_DIR}/sound_digital_click_1.s
    ${DATA_DIR}/sound_fizzle.s
    ${DATA_DIR}/sound_gravel.s
    ${DATA_DIR}/sound_dropitem.s
    ${DATA_DIR}/sound_scroll.s
    ${DATA_DIR}/sound_beep_error.s
    ${DATA_DIR}/sound_drone_beep.s
    ${DATA_DIR}/sound_laser1.s
    ${DATA_DIR}/sound_select.s
    ${DATA_DIR}/sound_openbag.s
    ${DATA_DIR}/sound_blaster.s
    ${DATA_DIR}/sound_tw_bell.s
    ${DATA_DIR}/sound_footstep1.s
    ${DATA_DIR}/sound_footstep2.s
    ${DATA_DIR}/sound_footstep3.s
    ${DATA_DIR}/sound_footstep4.s
    ${DATA_DIR}/sound_open_book.s
    ${DATA_DIR}/sound_explosion1.s
    ${DATA_DIR}/sound_typewriter.s
    ${DATA_DIR}/sound_explosion2.s)
endif()


if(NINTENDO_DS)
  set(SOURCES
    ${SOURCES})
elseif(GAMEBOY_ADVANCE)

  set(SOURCES
    ${SOURCES}
    ${SOURCE_DIR}/platform/gba/mixer.cpp
    ${SOURCE_DIR}/platform/gba/gba_platform.cpp
    ${SOURCE_DIR}/platform/gba/skyland_mgba.cpp
    ${SOURCE_DIR}/platform/gba/gbp_logo.cpp
    ${SOURCE_DIR}/platform/gba/memcpy.s
    ${SOURCE_DIR}/platform/gba/bios.s
    ${SOURCE_DIR}/platform/gba/filesystem.cpp
    ${SOURCE_DIR}/platform/gba/bootleg_cart.cpp

    ${SOURCE_DIR}/platform/gba/dlc_download.cpp)

elseif(PSP)

set(SOURCES
    ${SOURCES}
    ${SOURCE_DIR}/platform/psp/psp_platform.cpp)

else()
  set(SOURCES
    ${SOURCES}
    ${SOURCE_DIR}/platform/sdl/sdl_platform.cpp
    ${SOURCE_DIR}/platform/desktop/resource_path.cpp)
endif()


if(APPLE)
  set(EXECUTABLE_NAME Skyland)
  set(MACOSX_BUNDLE_ICON_FILE AppIcon.icns)
  set(MACOSX_BUNDLE_GUI_IDENTIFIER "io.skylandgame.skyland")
  add_executable(${EXECUTABLE_NAME} MACOSX_BUNDLE ${SOURCES})

elseif(GAMEBOY_ADVANCE)
  set(EXECUTABLE_NAME SkylandEngine)
  add_executable(${EXECUTABLE_NAME}
    ${SOURCES})
else()
  set(EXECUTABLE_NAME Skyland)
  add_executable(${EXECUTABLE_NAME}
    ${SOURCES})
endif()


set(IMAGE_INCLUDES "")
set(IMAGE_SPR_STUBS "")
set(IMAGE_TILE_STUBS "")
set(IMAGE_OVERLAY_STUBS "")
set(IMAGE_BACKGROUND_STUBS "")


macro(add_spritesheet filename mw mh flatten)

  set(IMAGE_INCLUDES ${IMAGE_INCLUDES}
    "\n#include \"data/${filename}.h\"\n//")

  set(IMAGE_SPR_STUBS ${IMAGE_SPR_STUBS}
    "\n    TEXTURE_INFO(${filename}, false),\n//")

  compile_image(${filename} ${mw} ${mh} ${flatten} 4 NO)
endmacro()


macro(add_tilesheet filename mw mh flatten compr)
  if(${flatten} STREQUAL "YES")
    set(FLATTENED_SUFFIX "_flattened")
  else()
    set(FLATTENED_SUFFIX "")
  endif()

  set(IMAGE_INCLUDES ${IMAGE_INCLUDES}
    "\n#include \"data/${filename}${FLATTENED_SUFFIX}.h\"\n//")

  set(SUB_COMPR "false")
  if(${compr} STREQUAL "YES")
    set(SUB_COMPR "true")
  endif()

  set(IMAGE_TILE_STUBS ${IMAGE_TILE_STUBS}
    "\n    TEXTURE_INFO(${filename}${FLATTENED_SUFFIX}, ${SUB_COMPR}),\n//")

  compile_image(${filename} ${mw} ${mh} ${flatten} 4 ${compr})
endmacro()


macro(add_overlay filename mw mh flatten compr)
  if(${flatten} STREQUAL "YES")
    set(FLATTENED_SUFFIX "_flattened")
  else()
    set(FLATTENED_SUFFIX "")
  endif()

  set(SUB_COMPR "false")
  if(${compr} STREQUAL "YES")
    set(SUB_COMPR "true")
  endif()

  set(IMAGE_INCLUDES ${IMAGE_INCLUDES}
    "\n#include \"data/${filename}${FLATTENED_SUFFIX}.h\"\n//")

  set(IMAGE_OVERLAY_STUBS ${IMAGE_OVERLAY_STUBS}
    "\n    TEXTURE_INFO(${filename}${FLATTENED_SUFFIX}, ${SUB_COMPR}),\n//")

  compile_image(${filename} ${mw} ${mh} ${flatten} 4 ${compr})
endmacro()


macro(add_background filename mw mh flatten compr)
  if(${flatten} STREQUAL "YES")
    set(FLATTENED_SUFFIX "_flattened")
  else()
    set(FLATTENED_SUFFIX "")
  endif()

  set(SUB_COMPR "false")
  if(${compr} STREQUAL "YES")
    set(SUB_COMPR "true")
  endif()

  set(IMAGE_INCLUDES ${IMAGE_INCLUDES}
    "\n#include \"data/${filename}${FLATTENED_SUFFIX}.h\"\n//")

  set(IMAGE_BACKGROUND_STUBS ${IMAGE_BACKGROUND_STUBS}
    "\n    TEXTURE_INFO(${filename}${FLATTENED_SUFFIX}, ${SUB_COMPR}),\n//")

  compile_image(${filename} ${mw} ${mh} ${flatten} 4 ${compr})
endmacro()


function(compile_image filename mw mh flatten bpp compr)
  message("rebuilding image...")
  if(${compr} STREQUAL "YES")
    set(COMPRESSION "-gzl")
  else()
    set(${COMPRESSION} "")
  endif()
  if(${flatten} STREQUAL "YES")
    add_custom_command(OUTPUT ${SOURCE_DIR}/data/${filename}_flattened.s
      COMMAND python3 prep_image.py ${ROOT_DIR} ${filename}.png yes
      COMMAND ${DEVKITPRO}/tools/bin/grit tmp/${filename}_flattened.png -pn16 -gB${bpp} -gTFF00FF ${COMPRESSION}
      COMMAND mv ${filename}_flattened.s ${SOURCE_DIR}/data/${filename}_flattened.s
      COMMAND mv ${filename}_flattened.h ${SOURCE_DIR}/data/${filename}_flattened.h
      DEPENDS ${IMAGE_DIR}/${filename}.png)

    add_custom_target(compile_image_${filename} DEPENDS ${SOURCE_DIR}/data/${filename}_flattened.s)

  elseif(${mw} STREQUAL "0" AND ${mh} STREQUAL "0")
    add_custom_command(OUTPUT ${SOURCE_DIR}/data/${filename}.s
      COMMAND python3 prep_image.py ${ROOT_DIR} ${filename}.png no
      COMMAND ${DEVKITPRO}/tools/bin/grit  tmp/${filename}.png -pn16 -gB${bpp} -gTFF00FF ${COMPRESSION}
      COMMAND mv ${filename}.s ${SOURCE_DIR}/data/${filename}.s
      COMMAND mv ${filename}.h ${SOURCE_DIR}/data/${filename}.h
      DEPENDS ${IMAGE_DIR}/${filename}.png)

    add_custom_target(compile_image_${filename} DEPENDS ${SOURCE_DIR}/data/${filename}.s)

  else()
    add_custom_command(OUTPUT ${SOURCE_DIR}/data/${filename}.s
      COMMAND python3 prep_image.py ${ROOT_DIR} ${filename}.png no
      COMMAND ${DEVKITPRO}/tools/bin/grit tmp/${filename}.png -pn16 -gB${bpp} -Mw ${mw} -Mh ${mh} -gTFF00FF ${COMPRESSION}
      COMMAND mv ${filename}.s ${SOURCE_DIR}/data/${filename}.s
      COMMAND mv ${filename}.h ${SOURCE_DIR}/data/${filename}.h
      DEPENDS ${IMAGE_DIR}/${filename}.png)

    add_custom_target(compile_image_${filename} DEPENDS ${SOURCE_DIR}/data/${filename}.s)

  endif()

  add_dependencies(${EXECUTABLE_NAME} compile_image_${filename})
endfunction()


if(APPLE OR UNIX OR GAMEBOY_ADVANCE OR NINTENDO_DS)

  set(SHARED_COMPILE_OPTIONS
    -O2
    ${G_COMPILE_FLAGS}
    -D__BLINDJUMP_ENABLE_LOGS)

  if(GAMEBOY_ADVANCE OR NINTENDO_DS)
    set(SHARED_COMPILE_OPTIONS
      ${SHARED_COMPILE_OPTIONS}
      -fno-exceptions)
  endif()

elseif(WIN32)

  set(SHARED_COMPILE_OPTIONS
    /W4)

  include_directories(${ROOT_DIR}/external/SFML-2.5.1/include/)

  set(SFML_LIB_DIR ${ROOT_DIR}/external/SFML-2.5.1/lib/Release)

  target_link_libraries(${EXECUTABLE_NAME}
    ${SFML_LIB_DIR}/sfml-graphics.lib
    ${SFML_LIB_DIR}/sfml-window.lib
    ${SFML_LIB_DIR}/sfml-system.lib
    ${SFML_LIB_DIR}/sfml-audio.lib
    ${SFML_LIB_DIR}/sfml-network.lib)

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${SFML_LIB_DIR}/sfml-graphics-2.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${SFML_LIB_DIR}/sfml-audio-2.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${SFML_LIB_DIR}/sfml-window-2.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${SFML_LIB_DIR}/sfml-network-2.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${SFML_LIB_DIR}/sfml-system-2.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)

   add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${ROOT_DIR}/external/SFML-2.5.1/extlibs/bin/x64/openal32.dll"
        $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>)
endif()


if(GAMEBOY_ADVANCE OR NINTENDO_DS)
  if(GBA_AUTOBUILD_IMG)
    add_spritesheet(spritesheet 2 4 NO)
    add_spritesheet(spritesheet_title_screen 2 4 NO)
    add_spritesheet(spritesheet_intro 2 4 NO)
    add_spritesheet(spritesheet_macro 2 4 NO)
    add_spritesheet(spritesheet_macroverse 2 4 NO)
    add_spritesheet(spritesheet_os 2 4 NO)
    add_tilesheet(tilesheet 2 2 NO NO)
    add_tilesheet(tilesheet_enemy_0 2 2 NO NO)
    add_tilesheet(tilesheet_enemy_0_interior 2 2 NO NO)
    add_tilesheet(tilesheet_interior 2 2 NO NO)
    add_tilesheet(tilesheet_world_map_backdrop 2 2 NO YES)
    add_tilesheet(tilesheet_world_map_framebuffer 2 2 NO YES)
    add_tilesheet(skyland_title_5 0 0 YES YES)
    add_tilesheet(skyland_title_4 0 0 YES YES)
    add_tilesheet(skyland_title_3 0 0 YES YES)
    add_tilesheet(skyland_title_2 0 0 YES YES)
    add_tilesheet(skyland_title_1 0 0 YES YES)
    add_tilesheet(skyland_title_0 0 0 YES YES)
    add_tilesheet(wallpaper 0 0 YES YES)
    add_tilesheet(boot_img 0 0 YES YES)
    add_tilesheet(savegame 0 0 YES YES)
    add_tilesheet(goblin_savegame 0 0 YES YES)
    add_tilesheet(sylph_savegame 0 0 YES YES)
    add_tilesheet(ending_img 0 0 YES YES)
    add_tilesheet(macro_rendertexture 0 0 NO NO)
    add_tilesheet(developer_mode 0 0 YES YES)
    add_tilesheet(zone_image_0 0 0 YES YES)
    add_tilesheet(zone_image_1 0 0 YES YES)
    add_tilesheet(zone_image_2 0 0 YES YES)
    add_tilesheet(zone_image_3 0 0 YES YES)
    add_tilesheet(zone_image_4 0 0 YES YES)
    add_tilesheet(datacarts 0 0 YES YES)
    add_tilesheet(glossary_factory_cg 0 0 YES YES)
    add_tilesheet(glossary_misc_cg 0 0 YES YES)
    add_tilesheet(glossary_decoration_cg 0 0 YES YES)
    add_tilesheet(glossary_power_cg 0 0 YES YES)
    add_tilesheet(glossary_wall_cg 0 0 YES YES)
    add_tilesheet(gba_health_safety_logo 2 2 YES YES)
    add_background(background 0 0 NO YES)
    add_background(background_macro 0 0 NO YES)
    add_background(background_macro_night 0 0 NO YES)
    add_background(background_title_screen 0 0 NO YES)
    add_overlay(colorProfiles 12 7 NO NO)
    add_overlay(flags 2 2 NO NO)
    add_overlay(character_art 4 4 NO NO)
    add_overlay(appendix 4 4 NO NO)
    add_overlay(paint_icons 2 2 NO NO)
    add_overlay(traded_core 0 0 YES YES)
    add_overlay(photo_village 0 0 YES YES)
    add_overlay(photo_goblin_stronghold 0 0 YES YES)
    add_overlay(photo_surface 0 0 YES YES)
    add_overlay(photo_market 0 0 YES YES)
    add_overlay(photo_disaster 0 0 YES YES)
    add_overlay(photo_incinerator 0 0 YES YES)
    add_overlay(overlay_datacart 0 0 NO YES)
    add_overlay(overlay_adventurelog 0 0 NO YES)
    add_overlay(overlay_textview 0 0 NO YES)
    add_overlay(stack_overflow 0 0 YES YES)
    add_overlay(overlay 0 0 NO NO)
    add_overlay(overlay_os 0 0 NO NO)
    add_overlay(overlay_qr 0 0 NO YES)
    add_overlay(overlay_gamespeed 0 0 NO YES)
    add_overlay(overlay_editor 0 0 NO YES)
    add_overlay(overlay_dialog 0 0 NO NO)
    add_overlay(overlay_dialog_fullscreen 0 0 NO YES)
    add_overlay(overlay_skyland_title 0 0 NO YES)
    add_overlay(charset 0 0 NO NO)
    add_overlay(charset_extended_0 0 0 NO NO)
    add_overlay(charset_extended_1 0 0 NO NO)
    add_overlay(overlay_world_map 0 0 NO YES)
    add_overlay(overlay_island_destroyed 0 0 NO YES)
    add_overlay(overlay_challenges 0 0 NO YES)
    add_overlay(overlay_large_numeral 0 0 NO YES)
    add_overlay(hint_infirmary 0 0 NO YES)
    add_overlay(hint_goblin 0 0 NO YES)
    add_overlay(hint_plunder 0 0 NO YES)
    add_overlay(hint_damaged_core 0 0 NO YES)

    configure_file(images.cpp.in ${SOURCE_DIR}/platform/gba/images.cpp)
    configure_file(images.cpp.in ${SOURCE_DIR}/platform/nds/images.cpp)
  endif()

endif()



if(NINTENDO_DS)

  set(NDS_SOURCE_DIR ${SOURCE_DIR}/platform/nds)

  # NOTE: For the gameboy advance, I simply appended the filesystem bin to the
  # end of the ROM, because the linker script specified a symbol for
  # __rom_end__. I haven't figured out (or spent much time figuring out) a good
  # solution for NDS, so I simply added a chain of custom targets, which rebuild
  # fs.bin
  file(GLOB_RECURSE SCRIPTS "${ROOT_DIR}/scripts/*.lisp")

  add_custom_command(OUTPUT ${NDS_SOURCE_DIR}/fs.bin
    COMMAND python3 encode_files.py
    COMMAND cp fs.bin ${NDS_SOURCE_DIR}/fs.bin
    DEPENDS ${SCRIPTS})
  add_custom_target(make_fs DEPENDS ${NDS_SOURCE_DIR}/fs.bin)

  add_dependencies(${EXECUTABLE_NAME} make_fs)


  set(CMAKE_AR $ENV{DEVKITARM}/bin/arm-none-eabi-ar)

  set(ARM9_FLAGS
    -mthumb
    -mthumb-interwork
    -march=armv5te
    -mtune=arm946e-s
    -D__NDS9__
    -D__NDS__)


  add_library(PLATFORM
    ${NDS_SOURCE_DIR}/filesystem.cpp
    ${NDS_SOURCE_DIR}/nds_platform.cpp)

  # libnds does a whole bunch of unfortunate things that're incompatible with
  # the -pedantic flag. I ended up needing to move the platform source file to a
  # separate static library and give it special (less restrictive) compilation
  # flags.
  target_compile_options(PLATFORM PRIVATE
    -O2
    -Wall
    -I/opt/devkitpro/libnds/include
    -DARM9
    ${ARM9_FLAGS})

  target_link_libraries(PLATFORM PRIVATE
    "${DEVKITPRO}/libnds/lib/libnds9.a")

  target_link_libraries(${EXECUTABLE_NAME} PRIVATE
    PLATFORM)

  target_compile_options(LISP PRIVATE
    ${ARM9_FLAGS})

  target_compile_options(FASTPATH PRIVATE
    ${ARM9_FLAGS})

  target_compile_options(${EXECUTABLE_NAME} PRIVATE
    ${ARM9_FLAGS})


  set(CMAKE_EXE_LINKER_FLAGS
    "-L/opt/devkitpro/libnds/lib/ -mthumb -mthumb-interwork -Wl,-Map,SkylandEngine.elf.map -specs=ds_arm9.specs"
    CACHE INTERNAL "" FORCE)


  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMENT "building cartridge"
    COMMAND ${DEVKITARM}/bin/arm-none-eabi-objcopy -O binary ${EXECUTABLE_NAME} SkylandEngine.arm9
    COMMAND ${DEVKITPRO}/tools/bin/ndstool -c Skyland.nds -9 SkylandEngine.arm9 -g YKWE 01 SKYLAND 0)


elseif(GAMEBOY_ADVANCE)

  # I am setting CMAKE_AR in the toolchain file, but for some reason, the
  # variable no longer exists within the cmakelists... which is strange, because
  # other variables set in the toolchain file seem to be persistent...
  set(CMAKE_AR $ENV{DEVKITARM}/bin/arm-none-eabi-ar)


  set(ARM7_THUMB_FLAGS
    -mthumb
    -mthumb-interwork
    -mcpu=arm7tdmi
    -mtune=arm7tdmi
    -D__GBA__)


  add_library(LIBGBA
    # NOTE: In order to use LIBGBA without gpl copyleft, the source code in
    # these files may not be changed! LIBGBA uses a modified gpl license, where
    # the authors waive the copyleft in cases where the build system is changed,
    # or where the un-modified code is statically linked. I don't believe that
    # I've made any changes that would constitute a derivative work, as I don't
    # know ARM assembly, I couldn't possibly have changed anything.
    #
    # The exact text:
    #
    # "1. Modifications to the build scripts and makefiles by themselves to
    # support a specific platform do not constitute a modified or derivative
    # work."
    #
    # "2. Static linking of applications to the libgba library does not
    # constitute a derivative work and does not require the author to provide
    # source code for the application or link their applications against a
    # user-supplied version of libgba."
    #
    # In summary, I changed the build system as described in 1., I am statically
    # linking to the sources as described in 2.
    #
    ${ROOT_DIR}/external/libgba/interrupt.c
    ${ROOT_DIR}/external/libgba/interrupt_dispatch.s
    ${ROOT_DIR}/external/libgba/reset.s
    ${SOURCE_DIR}/platform/gba/libc.c
    ${SOURCE_DIR}/platform/gba/send_multiboot.c
    ${SOURCE_DIR}/platform/gba/rumble.c)


  target_compile_options(LIBGBA PRIVATE
    -O2
    ${ARM7_THUMB_FLAGS}
    ${G_COMPILE_FLAGS})


  add_library(QR
    ${ROOT_DIR}/external/qr/qrcodegen.c)

  target_compile_options(QR PRIVATE
    -O2
    ${ARM7_THUMB_FLAGS})


  add_library(ARM_ROUTINES ${SOURCE_DIR}/platform/gba/gba_hotpath.cpp)

  target_compile_options(ARM_ROUTINES PRIVATE
    -g
    -Ofast
    -fno-rtti
    ${G_COMPILE_FLAGS})

  target_compile_options(ARM_ROUTINES PRIVATE
    -marm
    -mlong-calls
    -mthumb-interwork
    -mcpu=arm7tdmi
    -mtune=arm7tdmi)

  set(CMAKE_EXE_LINKER_FLAGS
    "-mthumb -mthumb-interwork -Wl,--trace-symbol=__getreent -Wl,--trace-symbol=_impure_data -Wl,-Map,SkylandEngine.elf.map -specs=gba.specs"
    CACHE INTERNAL "" FORCE)

  target_compile_options(LISP PRIVATE
    ${ARM7_THUMB_FLAGS}
    ${G_COMPILE_FLAGS})

  target_compile_options(FASTPATH PRIVATE
    -fno-rtti
    ${ARM7_THUMB_FLAGS}
    ${G_COMPILE_FLAGS})

  target_link_libraries(${EXECUTABLE_NAME} PUBLIC
    LIBGBA
    QR
    ARM_ROUTINES)

  target_compile_options(${EXECUTABLE_NAME} PRIVATE
    ${ARM7_THUMB_FLAGS}
    -fno-rtti
    ${G_COMPILE_FLAGS})

  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    COMMENT "building cartridge"
    COMMAND ${DEVKITARM}/bin/arm-none-eabi-objcopy -O binary ${EXECUTABLE_NAME} ${EXECUTABLE_NAME}.gba
    COMMAND ${DEVKITPRO}/tools/bin/gbafix -tSKYLAND -cSKLD -r0 -mEB ${EXECUTABLE_NAME}.gba)

elseif(APPLE)

  # Set minimum macOS version to avoid version mismatch warnings
  set(CMAKE_OSX_DEPLOYMENT_TARGET "13.3" CACHE STRING "Minimum macOS deployment version")

  target_include_directories(${EXECUTABLE_NAME} PRIVATE
    /opt/homebrew/include)

  target_link_directories(${EXECUTABLE_NAME} PRIVATE
    /opt/homebrew/lib)

  find_package(SDL2_image REQUIRED)
  target_link_libraries(${EXECUTABLE_NAME} PRIVATE SDL2_image::SDL2_image)

  target_link_libraries(${EXECUTABLE_NAME} PRIVATE
    "-framework Cocoa -framework SDL2")

  set_target_properties(${EXECUTABLE_NAME}
    PROPERTIES
    LINK_FLAGS "-Wl,-F/Library/Frameworks"
    MACOSX_BUNDLE TRUE
    MACOSX_BUNDLE_BUNDLE_NAME "Skyland"
    MACOSX_BUNDLE_GUI_IDENTIFIER "com.evanbowman.skyland"
    MACOSX_BUNDLE_BUNDLE_VERSION "1.0"
    MACOSX_BUNDLE_SHORT_VERSION_STRING "1.0")

  include_directories("/usr/local/include/")

  # Copy resources and frameworks into the app bundle
  add_custom_target(pkg ALL
    COMMAND mkdir -p ${EXECUTABLE_NAME}.app/Contents/Resources
    COMMAND mkdir -p ${EXECUTABLE_NAME}.app/Contents/Frameworks
    COMMAND python3 ../tools/encode_files.py
    COMMAND cp -r ${ROOT_DIR}/lisp_symtab.dat ${EXECUTABLE_NAME}.app/Contents/Resources/
    COMMAND cp -r ${ROOT_DIR}/lisp_constant_tab.dat ${EXECUTABLE_NAME}.app/Contents/Resources/
    COMMAND cp -r ${ROOT_DIR}/images/ ${EXECUTABLE_NAME}.app/Contents/Resources/images/
    COMMAND cp -r ${ROOT_DIR}/images/AppIcon.icns ${EXECUTABLE_NAME}.app/Contents/Resources/
    COMMAND cp -r ${ROOT_DIR}/shaders/ ${EXECUTABLE_NAME}.app/Contents/Resources/shaders/
    COMMAND cp -r ${ROOT_DIR}/sounds/ ${EXECUTABLE_NAME}.app/Contents/Resources/sounds/
    COMMAND cp -r ${ROOT_DIR}/scripts/ ${EXECUTABLE_NAME}.app/Contents/Resources/scripts/
    COMMAND cp -r ${ROOT_DIR}/strings/ ${EXECUTABLE_NAME}.app/Contents/Resources/strings/
    # Copy SDL2 framework
    COMMAND cp -r /Library/Frameworks/SDL2.framework ${EXECUTABLE_NAME}.app/Contents/Frameworks
    # Copy SDL2_image library - remove old ones first to avoid permission issues
    COMMAND rm -f ${EXECUTABLE_NAME}.app/Contents/Frameworks/libSDL2_image*.dylib
    COMMAND cp /opt/homebrew/opt/sdl2_image/lib/libSDL2_image*.dylib ${EXECUTABLE_NAME}.app/Contents/Frameworks/
    DEPENDS ${EXECUTABLE_NAME}
    COMMENT "Copying resources into app bundle")

  # Post-build: Fix library paths for distribution
  add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD
    # Fix SDL2_image's dependency on SDL2
    COMMAND bash -c "for lib in ${EXECUTABLE_NAME}.app/Contents/Frameworks/libSDL2_image*.dylib; do install_name_tool -change '@rpath/SDL2.framework/Versions/A/SDL2' '@executable_path/../Frameworks/SDL2.framework/Versions/A/SDL2' \"$${lib}\" 2>/dev/null || true; done"

    # Fix executable's dependency on SDL2
    COMMAND install_name_tool -change
      "@rpath/SDL2.framework/Versions/A/SDL2"
      "@executable_path/../Frameworks/SDL2.framework/Versions/A/SDL2"
      ${EXECUTABLE_NAME}.app/Contents/MacOS/${EXECUTABLE_NAME}

    # Fix executable's dependency on SDL2_image (handle potential version number variations)
    COMMAND bash -c "install_name_tool -change /opt/homebrew/opt/sdl2_image/lib/libSDL2_image-2.0.0.dylib '@executable_path/../Frameworks/libSDL2_image-2.0.0.dylib' ${EXECUTABLE_NAME}.app/Contents/MacOS/${EXECUTABLE_NAME} 2>/dev/null || true"
    COMMAND bash -c "install_name_tool -change /opt/homebrew/lib/libSDL2_image-2.0.0.dylib '@executable_path/../Frameworks/libSDL2_image-2.0.0.dylib' ${EXECUTABLE_NAME}.app/Contents/MacOS/${EXECUTABLE_NAME} 2>/dev/null || true"
    COMMAND bash -c "install_name_tool -change /opt/homebrew/Cellar/sdl2_image/2.8.8/lib/libSDL2_image.dylib '@executable_path/../Frameworks/libSDL2_image.dylib' ${EXECUTABLE_NAME}.app/Contents/MacOS/${EXECUTABLE_NAME} 2>/dev/null || true"

    # Verify the final result
    COMMAND otool -L ${EXECUTABLE_NAME}.app/Contents/MacOS/${EXECUTABLE_NAME}

    COMMENT "Fixing library paths for macOS distribution"
    VERBATIM)

elseif(PSP)

  set(PSP_FLAGS
    -I/usr/local/pspdev/psp/sdk/include
    -D_PSP_FW_VERSION=500
    -G0
    -g3
    -mpreferred-stack-boundary=4
    -ffast-math
    -gdwarf-2
    -D__PSP__
    -I.
    -I/usr/local/pspdev/psp/sdk/ninclude)

  target_compile_options(LISP PRIVATE
    ${PSP_FLAGS}
    ${G_COMPILE_FLAGS})

  target_compile_options(FASTPATH PRIVATE
    ${PSP_FLAGS}
    ${G_COMPILE_FLAGS})

  target_compile_options(${EXECUTABLE_NAME} PRIVATE
    ${PSP_FLAGS}
    -fno-rtti
    ${G_COMPILE_FLAGS})

  add_library(QR
    ${ROOT_DIR}/external/qr/qrcodegen.c)

  target_compile_options(QR PRIVATE
    -O2
    ${PSP_FLAGS})

  target_link_libraries(${EXECUTABLE_NAME} PRIVATE
    QR)

  set(CMAKE_EXE_LINKER_FLAGS
    "-I. -I/usr/local/pspdev/psp/sdk/include -Os -G0 -g3 -mpreferred-stack-boundary=4 -ffast-math -gdwarf-2 -D_PSP_FW_VERSION=500  -L. -L/usr/local/pspdev/psp/sdk/lib -specs=/usr/local/pspdev/psp/sdk/lib/prxspecs -Wl,-q,-T/usr/local/pspdev/psp/sdk/lib/linkfile.prx -lstdc++ -lpspgum -lpspgu -lpng -ljpeg -lz -lm  -lpspvram -lpsprtc -lpspusb -lpsputility -lpspusbcam -lpspjpeg -lpspdebug -lpspdisplay -lpspge -lpspctrl -lpspsdk -lc -lpspnet -lpspnet_inet -lpspnet_apctl -lpspnet_resolver -lpsputility -lpspuser -lpspkernel"
    CACHE INTERNAL "" FORCE)


elseif(UNIX)

  add_library(QR
    ${ROOT_DIR}/external/qr/qrcodegen.c)

  target_link_libraries(Skyland PRIVATE
    -lpthread
    QR)

endif()



target_precompile_headers(${EXECUTABLE_NAME}
  PRIVATE
  <algorithm>
  <array>
  <ciso646>
  <initializer_list>
  <iterator>
  <limits>
  <memory>
  <new>
  <optional>
  <stddef.h>
  <tuple>
  <type_traits>
  <utility>
  <variant>
  ../source/unicode.hpp
  ../source/allocator.hpp
  ../source/containers/vector.hpp
  ../source/graphics/overlay.hpp
  ../source/globals.hpp
  ../source/memory/buffer.hpp
  ../source/script/lisp.hpp
  ../source/number/numeric.hpp
  ../source/number/fixnum.hpp
  ../source/skyland/room.hpp
  ../source/skyland/island.hpp
  ../source/skyland/entity.hpp
  ../source/skyland/timeStreamEvent.hpp
  ../source/platform/platform.hpp
  ../source/skyland/skyland.hpp)



target_compile_options(${EXECUTABLE_NAME} PRIVATE
  ${SHARED_COMPILE_OPTIONS})



target_link_libraries(${EXECUTABLE_NAME} PRIVATE
  LISP
  FASTPATH)



file(GLOB_RECURSE SOURCES "${SOURCE_DIR}/*.cpp")
file(GLOB_RECURSE HEADERS "${SOURCE_DIR}/*.hpp")

add_custom_target(clang-format
  COMMAND clang-format --style=file -i ${SOURCES} ${HEADERS}
  COMMENT "Running clang-format"
  VERBATIM)
