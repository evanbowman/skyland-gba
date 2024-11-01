with open('SkylandEngine.gba', 'rb') as engine_file:
    with open('fs.bin', 'rb') as resource_bundle:
        with open('Skyland.gba', 'wb') as output_rom:
            output_rom.write(engine_file.read())
            output_rom.write(resource_bundle.read())
